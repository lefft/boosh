__author__ = 'Aaron_Jackson_Januar24'

#!/usr/bin/python
# -*- coding: utf-8 -*-

#edited Jan-07-2015

########################################################
# light edits by tim leffel, apr9/2016:                #
# some colnames have been changed from "x y" to "x.y", #
# because of how R handles spaces in colnames          #
########################################################

import os
import csv


path = r'/Users/timothyleffel/Google Drive/work UoC/vwp_imprecise/2016-november-sharing/raw-gazedata-files'

# we will place our parsed experiment data in a specific folder that will be created below
# if the folder does not exist, it will be created
desiredFolderPath = os.path.join(os.getcwd(), 'parsed')
if not os.path.exists(desiredFolderPath):
    os.makedirs(desiredFolderPath)
#change working directory to the new folder path, which is where we want to save our files
os.chdir(desiredFolderPath)

logfile = open('log', 'w')

experimentFiles = [files for files in os.listdir(path) if files.endswith('.gazedata')]
for currExperimentFile in experimentFiles:
    #check to ensure we have a unique name for our experiment file

    new_name = currExperimentFile[:-4]+'_parsed.csv'#'eyeTracker-data_v2_subj%d.csv' % counter

    with open(new_name,"wb") as newFile:

        out = csv.writer(newFile, delimiter=",")

        reader = csv.reader(open(os.path.join(path,currExperimentFile),"rU"), dialect=csv.excel_tab)
        line_index = 0

        prevTimeStamp = 0
        prevItem = ""

        adjectiveCount = 0
        nounCount = 0
        adjectiveFound = False
        nounFound = False

        curr_trial_pos = '0'
        prestimulussoundBeginTimestamp = 0
        currentObject = ""

        for (e, line) in enumerate(reader):
            item = str(line[0]).lower().strip().split(",")
            #for every line, we want to reset our variables to 0
            target = 0
            contrast = 0
            competitor = 0
            distractor = 0

            #along with the time slices
            previewTimeSlice = 0
            adjectiveTimeSlice = 0
            nounTimeSlice = 0



            previewTimeSliceTimestamp = ""
            adjectiveTimeSliceTimestamp = ""
            nounTimeSliceTimestamp = ""



            #if our line_index is 0 then the line being examined is the header.
              #line_index is the line number of the file
            #this chunk of code gets the positions of the relevent columns in the file output by the eye-tracker
            if line_index == 0:
                subject_index = item.index('subject')
                condition_index = item.index('condition')
                trial_type_index = item.index('trialtype')
                trial_id_index = item.index('trialid')
                target_color_index = item.index('target.color')
                adjective_onset_index = item.index('adjectiveonset')
                noun_onset_index = item.index('nounonset')
                current_object_index = item.index('currentobject')
                aoi_stimulus_index = item.index('aoistimulus')
                item_id_index = item.index('itemid')
                adjective_id = item.index('adjectiveid')
                timestamp_sec_index = item.index('timestampsec')
                timestamp_microsec_index = item.index('timestampmicrosec')
                target_pos_index = item.index('targetpos')
                competitor_index = item.index('competitorpos')
                distractor1_index = item.index('distractor1pos')
                distractor_contrast_index = item.index('distractor2pos')
                adjective_type_index = item.index("adjectivetype")
                target_noun_index = item.index("target.noun")
                target_adjective_index = item.index("target.adjective")
                right_eye_validity_index = item.index("validityrighteye")
                line_index += 1

                #this chunk of code writes the header for the new data file

                # the items in green are variables not referenced in the eye-tracker data file
                # the order of the header items must be the same below.
                header = ['trialPosition',
                          item[subject_index],
                          item[trial_id_index],
                          item[condition_index],
                          item[trial_type_index],
                          item[adjective_type_index],
                          item[adjective_onset_index],
                          item[noun_onset_index],
                          item[item_id_index],
                          item[target_noun_index],
                          item[target_adjective_index],
                          item[target_color_index],
                          item[timestamp_sec_index],
                          item[timestamp_microsec_index],
                          item[right_eye_validity_index],
                          'target',
                          'competitor',
                          'distractor/contrast',
                          'distractor',
                          'preview window',
                          'adjective window',
                          'noun window']
                            #unused columns
    #                      'onset of preview time slice',
    #                      'onset of adjective-onset timeslice',
    #                      'onset of noun-onset timeslice']
                out.writerow(header)

            #check to see if a slide was onscreen for this row.
            elif item[current_object_index] != "":

                print>>logfile, 'e', e, 'item[trial_id_index]', repr(item[trial_id_index]), 'curr_trial_pos', repr(curr_trial_pos)

                #check to see if we are examining a different trial for this row
                if item[trial_id_index] != curr_trial_pos:

                    print>>logfile, '===== new trial now =>', 
                    print>>logfile,  'e', e, 'item[trial_id_index]', repr(item[trial_id_index]), 'curr_trial_pos', repr(curr_trial_pos)

                    curr_trial_pos = str(item[trial_id_index])

                    print>>logfile,  '    after assignment:', 'item[trial_id_index]', repr(item[trial_id_index]), 'curr_trial_pos', repr(curr_trial_pos)

                    if (adjectiveCount != 72) or (nounCount != 72):
                        print ' NOT YET 72!!! trial no', int(curr_trial_pos) - 1

                    adjectiveCount = 0
                    nounCount = 0

                    nounFound = False
                    adjectiveFound = False

                    adjectiveDone = False
                    nounDone = False

                #convert seconds to microseconds
                currTimeStamp = (int(item[timestamp_sec_index])*1000000)+int(item[timestamp_microsec_index])


                print>>logfile, 'item[current_object_index]', repr(item[current_object_index]), 'currentObject', currentObject
                if item[current_object_index] == 'prestimulussound' and currentObject == 'prestimulus':
                    prestimulussoundBeginTimestamp = currTimeStamp
                    print>>logfile, '**** switch from prestimulussound to prestimulus', prestimulussoundBeginTimestamp
                currentObject = item[current_object_index]

                #check to see if we are examining a different slide then the one in the previous row
                #if so, mark the current slide as the previous item and update the timestamp

                if prevItem != item[current_object_index]:
                    prevItem = item[current_object_index]
                    prevTimeStamp = currTimeStamp

                # check to see if we are examining the prestimulus
                if item[current_object_index] == 'prestimulus':
                    previewTimeSlice = 1
                    previewTimeSliceTimestamp = prevTimeStamp

                # check to see if we are examining a timestamp less then greater then the adjective onset
                # we do this by seeing if...
                # 1. the current time stamp is greater than: the timestamp recorded at the onset of the slide *plus* the onset
                # 2. the current time stamp is lower than: the timestamp recorded at the onset of the slide *plus* the adjective onset, *plus* 1200 miliseconds

                print>>logfile, 'int(item[adjective_onset_index])', int(item[adjective_onset_index])
                if (not adjectiveDone) and (not adjectiveFound) and \
                   (item[current_object_index] == 'prestimulussound') and \
                   (currTimeStamp > (prestimulussoundBeginTimestamp+(int(item[adjective_onset_index])*1000))) and \
                   (item[adjective_onset_index] != '0'):
                    #if this is true, we are in our adjective time slice
                    adjectiveFound = True

                if (not adjectiveDone) and adjectiveFound and (0 <= adjectiveCount < 72):
                    adjectiveTimeSlice = 1
                    #adjectiveTimeSliceTimestamp = prevTimeStamp
                    adjectiveCount += 1

                    print>>logfile,  'trial', item[trial_id_index], 'adjectiveCounter', adjectiveCount

                if adjectiveCount >= 72:
                    adjectiveFound = False
                    adjectiveDone = True

                # check to see if we are examining a timestamp less then greater then the noun onset
                # we do this by seeing if
                # 1. the current time stamp is greater than: the timestamp recorded at the onset of the slide *plus* the onset
                # 2. the current time stamp is lower than: the timestamp recorded at the onset of the slide *plus* the noun onset, *plus* 1200 miliseconds

                print>>logfile, 'int(item[noun_onset_index])', int(item[noun_onset_index])
                if (not nounDone) and (not nounFound) and \
                   (item[current_object_index] == 'prestimulussound') and \
                   (currTimeStamp > (prestimulussoundBeginTimestamp+(int(item[noun_onset_index])*1000))) and \
                   (item[noun_onset_index] != '0'):
                    nounFound = True

                if (not nounDone) and nounFound and (0 <= nounCount < 72):
                    nounTimeSlice = 1
                   #nounTimeSliceTimestamp = prevTimeStamp
                    nounCount += 1
                    print>>logfile,  'trial', item[trial_id_index], 'nounCounter', nounCount

                if nounCount >= 72:
                    nounFound = False
                    nounDone = True

                #if the validity of the right eye is 4, record the gaze data
                if item[right_eye_validity_index] != '4':
                    #the aoi_stimulus represents what was being fixated on. we check to see what this is equal to.
                    #for example, if the aoi_stimulus is equal to the target, then the target will be set to 1
                    if item[aoi_stimulus_index] == item[target_pos_index]:
                        target = 1
                    elif item[aoi_stimulus_index] == item[competitor_index]:
                        competitor = 1
                    elif item[aoi_stimulus_index] == item[distractor_contrast_index]:
                        contrast = 1
                    elif item[aoi_stimulus_index] == item[distractor1_index]:
                        distractor = 1
                #if the right eye validity is 4, then the gaze was lost, and -1 will be written for all picture columns
                elif item[right_eye_validity_index] == '4':
                    target = -1
                    competitor = -1
                    contrast = -1
                    distractor = -1

                #check to see if we are in one of our relevent time slices
                #if we are in a relevent time slice, write the row
                if previewTimeSlice == 1 or adjectiveTimeSlice == 1 or nounTimeSlice == 1:
                    #this chunk of code writes the line in our new data file
                    #the order of items here must match the order of items in the header
                    out.writerow([curr_trial_pos,
                                  item[subject_index],
                                  item[trial_id_index],
                                  item[condition_index],
                                  item[trial_type_index],
                                  item[adjective_type_index],
                                  item[adjective_onset_index],
                                  item[noun_onset_index],
                                  item[item_id_index],
                                  item[target_noun_index],
                                  item[target_adjective_index],
                                  item[target_color_index],
                                  item[timestamp_sec_index],
                                  item[timestamp_microsec_index],
                                  item[right_eye_validity_index],
                                  target,
                                  competitor,
                                  contrast,
                                  distractor,
                                  previewTimeSlice,
                                  adjectiveTimeSlice,
                                  nounTimeSlice])
                                    # unused columns
    #                              previewTimeSliceTimestamp,
    #                              adjectiveTimeSliceTimestamp,
    #                              nounTimeSliceTimestamp])

        newFile.flush()
        newFile.close()


logfile.close()


