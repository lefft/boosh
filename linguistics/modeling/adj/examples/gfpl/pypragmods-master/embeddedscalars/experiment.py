#!/usr/bin/env python

"""
Experiment analysis code for the paper. Geared toward processing and
summarizing embedded-scalars-experiment-results.csv The main method
does basic summary analysis.
"""

__author__ = "Christopher Potts"
__version__ = "2.0"
__license__ = "GNU general public license, version 3"
__maintainer__ = "Christopher Potts"
__email__ = "See the author's website"


from collections import defaultdict, Counter
import csv
from itertools import product
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats

import pypragmods.embeddedscalars.bootstrap
from pypragmods.embeddedscalars.settings import *

######################################################################
# Clas for modeling experimental items (rows in the table):

class Item:
    def __init__(self, data, response_transformation=(lambda x : int(x))):
        # Attributes for the underlying CSV:
        #
        # workerid,
        # Answer.language,
        # player1color,
        # player2color,
        # player3color,
        # sentence,
        # condition,
        # conditionOrder,
        # trialnum,
        # response,
        # rt,
        # trainingCorrect
        self.data = data
        for key, val in list(self.data.items()):
            key = key.replace(".", "_")
            if key in ('response',):
                val = response_transformation(val)
            elif key in ('trialnum', 'rt'):
                val = int(val)
            elif key == 'trainingCorrect' and val == 'NA':
                val = None
            setattr(self, key, val)
        # For correspondence with the modeling:
        self.condition_norm = CONDITION_MAP.get(self.condition, None)
        self.formula = SENTENCES.get(self.sentence, None)

    def __str__(self):
        return str(self.d)

######################################################################
# Class for the entire experiment, built from the spreadsheet:
    
class Experiment:
    def __init__(self,
            src_filename=BINARY_EXPERIMENT_SRC_FILENAME,
            response_transformation=(lambda x : int(x))):       
        self.src_filename = src_filename
        self.response_transformation = response_transformation
        self.data = [Item(d, response_transformation=response_transformation)
                     for d in csv.DictReader(open(src_filename))]
        self.targets = defaultdict(lambda : defaultdict(list))
        self.get_target_responses()

    def get_target_responses(self):        
        for item in self.data:
           if item.formula:
               self.targets[item.formula][item.condition_norm].append(item.response)

    def sample_target_responses(self):
        """For bootstrapping confidence intervals, build a simulated data
        set with by-subjects sampling"""
        # Organize the data by workerid:
        byworker = defaultdict(list)
        for item in self.data:
            if item.formula:
                byworker[item.workerid].append(
                    (item.formula, item.condition_norm, item.response))
        # Sample the usual number of workers with replacement:
        size = len(byworker)
        workerids = np.random.choice(list(byworker.keys()), size=size, replace=True, p=None)
        # Build self.targets with this subsample:
        self.targets = defaultdict(lambda : defaultdict(list))
        for workerid in workerids:
            for formula, condition_norm, response in byworker[workerid]:
                self.targets[formula][condition_norm].append(response)

    def target_means(self):
        return self.target_analysis(func=np.mean)

    def target_means2matrix(self, rnames, cnames):
        return self.target_values2matrix(rnames, cnames, self.target_means())

    def target_cis2matrix(self, rnames, cnames):
        return self.target_values2matrix(rnames, cnames, self.target_cis())
        
    def target_cis(self):        
        return self.target_analysis(func=self.get_ci)

    def get_ci(self, vals):
        if len(set(vals)) == 1:
            return (vals[0], vals[0])
        # In case bootstrap.py is missing or not working:
        # loc = np.mean(vals)
        # scale = np.std(vals) / np.sqrt(len(vals))
        # return stats.t.interval(0.95, len(vals)-1, loc=loc, scale=scale)        
        return bootstrap.ci(vals, method='bca')
    
    def target_analysis(self, func=np.mean):
        mu = defaultdict(lambda : defaultdict(list))
        for form, cond_dict in list(self.targets.items()):
            for cond, vals in list(cond_dict.items()):
                mu[form][cond] = func(vals)
        return mu
        
    def target_values2matrix(self, rnames, cnames, value_dict):        
        mat = []
        for i, rname in enumerate(rnames):
            row = []
            for j, cname in enumerate(cnames):
                row.append(value_dict[rname][cname])
            mat.append(row)
        return mat

    def pairwise_comparison_test(self, sentence, w1, w2, test=stats.mannwhitneyu):
        phi = SENTENCES[sentence]
        x = None
        y = None
        if isinstance(w1, list) or isinstance(w1, tuple):
            x = [a for w in w1 for a in self.targets[phi][w]]
        else:
            x = self.targets[phi][w1]            
        if isinstance(w2, list) or isinstance(w2, tuple):
            y = [a for w in w2 for a in self.targets[phi][w]]
        else:
            y = self.targets[phi][w2]
        return test(x, y)

    ##################################################################
    # Experimental report

    def experimental_report(self):
        print(("=" * 70))
        print(("Experimental report for", self.src_filename))
        print(('\nSubjects:', self.subject_count()))
        print('\nResponses per sentence:')
        for phi, count in list(self.target_sentence_response_counts().items()):
            print("\t{} {}".format(phi, count))
        print('\nResponses per sentence--condition pair:')
        for key, val in list(self.target_sentence_condition_response_summary().items()):
            print("\t{} {}".format(key, val))
        print('\nCondition presentation')
        for key, vals in list(self.condition_presentation().items()):
            print("\t{} {}".format(
                key, "; ".join(["%s: %s" % cond_count
                                for cond_count in list(vals.items())])))
        self.plot_response_distribution()
                 
    def subject_count(self):
        return len(set(item.workerid for item in self.data))        

    def target_sentence_response_counts(self):        
        return {phi: sum(len(x) for x in list(cond_dist.values()))
                for phi, cond_dist in list(self.targets.items())}

    def target_sentence_condition_response_summary(self):
        counts = self.target_analysis(func=len)
        counts = [x for cond_dist in list(counts.values())
                  for x in list(cond_dist.values())]
        return {'min': min(counts),
                'max': max(counts),
                'mean': np.mean(counts),
                'median': np.median(counts)}

    def condition_presentation(self):
        d = defaultdict(lambda : defaultdict(int))
        for item in self.data:
            if item.formula:
                d[item.condition_norm][item.conditionOrder] += 1
        return d

    ##################################################################
    # Plot

    def plot_response_distribution(self):
        all_responses = Counter([item.response for item in self.data])
        all_counts = [all_responses.get(x, 0.0) for x in range(1,8)]
        fig, axarray = plt.subplots(nrows=1, ncols=2)
        ax1, ax2 = axarray
        pos = np.arange(1.0, 8.0, 1.0)
        barwidth = 1.0
        ax1.bar(pos, all_counts, barwidth)       
        ax1.set_title('All items')
        ax1.set_xlabel('Response category')
        ax1.set_ylabel('Count')
        target_responses = Counter([x for cond_dist in list(self.targets.values())
                                    for vals in list(cond_dist.values()) for x in vals])
        target_counts = [target_responses.get(x, 0.0) for x in range(1,8)]
        ax2.bar(pos, target_counts, barwidth)
        ax2.set_title('Target items')
        ax2.set_xlabel('Response category')
        ax2.set_ylabel('Count')
        plt.show()
                
    def plot_targets(self,
            output_filename=None,
            all_x_labels=False,
            xlim=[0.0, 7.0],
            xticks=list(range(1,8)),
            xlabel="Human responses"):
        rnames = sorted(self.targets.keys())
        mat = self.target_means2matrix(rnames, CONDITIONS)
        confidence_intervals = self.target_cis2matrix(rnames, CONDITIONS)                        
        # Orientation:
        barwidth = 1.0
        pos = np.arange(0.0, len(CONDITIONS)*barwidth, barwidth)
        ylim = [0.0, len(CONDITIONS)*barwidth]
        yticks = pos+(barwidth/2.0)
        # Reversal for preferred condition order:
        yticklabels = [r'\texttt{%s}' % s for s in CONDITIONS[::-1]]
        titles = [TITLES[s] for s in rnames]
        titles = [r"\emph{%s}" % s for s in titles]
        # Sizing:        
        nrows = 3
        ncols = 3
        axis_height = 4
        axis_width = 4
        title_size = 20
        labsize = 20
        xtick_labelsize = 16
        ytick_labelsize = 16
        # Basic figure dimensions and design:
        fig, axarray = plt.subplots(nrows=nrows, ncols=ncols)
        fig.set_figheight(axis_height*nrows)
        fig.set_figwidth(axis_width*ncols)
        fig.subplots_adjust(wspace=0.3)
        fig.text(0.5, 0.05, xlabel, ha='center', va='center', fontsize=labsize)
        fig.text(0.05, 0.5, 'World', ha='center', va='center', rotation='vertical', fontsize=labsize)
        # Axes:
        indices = list(product(list(range(nrows)), list(range(ncols))))    
        for i, row in enumerate(mat):
            row = row[::-1] # Reversal for preferred condition order.
            axindex = indices[i]
            ax = axarray[axindex]
            ax.tick_params(
                axis='both',
                which='both',
                bottom='off',
                left='off',
                top='off',
                right='off',
                labelbottom='on')
            ax.barh(pos, row, barwidth, color=colors[0])
            ax.set_title(titles[i], fontsize=title_size)
            # x-axis
            ax.set_xlim(xlim)
            ax.set_xticks(xticks)
            # x-axis labels only for bottom row to avoid clutter:
            if axindex[0] == 2 or all_x_labels:
                ax.set_xticklabels(
                    xticks, fontsize=xtick_labelsize, color='black')
            else:
                ax.set_xticklabels([])
            # y-axis:
            ax.set_ylim(ylim)
            ax.set_yticks(yticks)            
            ax.set_yticklabels(
                yticklabels,
                fontsize=ytick_labelsize,
                rotation='horizontal',
                color='black')
            # Confidence intervals:
            cis = confidence_intervals[i]
            cis = cis[::-1]  # Reversal for preferred condition order.
            for j, xpos in enumerate(pos):
                xpos = xpos+(barwidth/2.0)
                ax.plot(cis[j], [xpos, xpos], linewidth=2, color='#555555') 
        # Output:
        if output_filename:
            plt.savefig(output_filename, bbox_inches='tight')
        else:
            plt.show()

######################################################################
    
if __name__ == '__main__':

    exp = Experiment(src_filename=BINARY_EXPERIMENT_SRC_FILENAME,
                     response_transformation=(lambda x : 1.0 if x=='T' else 0.0))
    #exp.plot_targets(output_filename=EXPERIMENT_SRC_FILENAME.replace('.csv', '.pdf'))
    exp.experimental_report()


