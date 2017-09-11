### notes to self, links to revisit, etc.

##### linxe


- [trading/finance tute](https://www.datacamp.com/community/tutorials/finance-python-trading) w python

- kyle rawlins [lambda notebooks](https://github.com/rawlins/lambda-notebook) system for writing fragments in a semantics-y feeling python extension (good for comparison against my efforts in r -- prob shd swap notes!)

- [stan examples](https://github.com/stan-dev/example-models/tree/master/Bayesian_Cognitive_Modeling) for bayesian models in cog sci -- very nice exx

- [faq on glmm](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html) (useful for reference)

- [cryptocurrency exploration in r](http://www.r-chart.com/2017/07/investigating-cryptocurrencies-using-r.html)

- [setting up rstudio on aws ec2](https://www.youtube.com/watch?v=rkDwPQuqP0g) (current of june 2017)

- yayyy, [ggjoy!!!](https://cran.r-project.org/web/packages/ggjoy/vignettes/introduction.html)

- [bayesian parameter estimation](https://www.youtube.com/watch?v=2_eFIyrOdJc) (and short [follow-up](https://youtu.be/a402ek-8oco) about the math of normalizing)

- stock prediction challenge [video](https://www.youtube.com/watch?v=ftMq5ps503w&feature=youtu.be) and accompanying [code](https://github.com/llSourcell/How-to-Predict-Stock-Prices-Easily-Demo) and a couple nice responses [here](https://github.com/ciurana2016/predict_stock_py) and [here](https://github.com/Avhirup/Stock-Market-Prediction-Challenge); see also [this post](http://machinelearningmastery.com/time-series-prediction-with-deep-learning-in-python-with-keras/)

- [game of life sim](https://www.youtube.com/watch?v=bNsrHRJQdKo) (quick)

- [jupyter tute](https://www.datacamp.com/community/tutorials/tutorial-jupyter-notebook), including how to mix r/py inline (nice for e.g. interfacing pandas + ggplot2)

- [jupyter notebooks](http://nbviewer.jupyter.org/github/nealcaren/workshop_2014/tree/master/notebooks/) for twitter/text mining workshop from a couple years ago (useful for hmc work -- inspect)

- [gh repo](https://github.com/ptwobrussell/Mining-the-Social-Web-2nd-Edition) for mining the social web oreilly book

- some [cool notebooks](https://github.com/jupyter/jupyter/wiki/A-gallery-of-interesting-Jupyter-Notebooks) to procrastinate by reading(*??)

- a little book of R for bayesian statistics [book](http://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/) seems to be in-progress (v nice so far!)

- [tensorflow playground](http://playground.tensorflow.org/)

- bayesian linear regression w `pyMC`: [v nice blog post](https://dsaber.com/2014/05/28/bayesian-regression-with-pymc-a-brief-tutorial/)

- m-e logistic regression w `pyMC`: [same dude, also v nice](https://dsaber.com/2016/08/27/analyze-your-experiment-with-a-multilevel-logistic-regression-using-pymc3/) (note: previous post sufficient background for this one)

- tips for facebook data stuff w python [here](http://www.kdnuggets.com/2017/06/6-interesting-things-facebook-python.html)

- `webPPL` textbook to work thru/see if necessary to work thru: [here](https://probmods.org/chapters/02-generative-models.html)

- [ppl intro](https://github.com/GalvanizeOpenSource/probabilistic-programming-intro)

- [bayesian item response theory](http://austinrochford.com/posts/2017-04-04-nba-irt.html) -- with nba data/examples

- [sk learn examples](http://scikit-learn.org/stable/auto_examples/index.html)

- [nice explanation of how MCMC works](https://eight2late.wordpress.com/2011/02/25/the-drunkard%E2%80%99s-dartboard-an-intuitive-explanation-of-monte-carlo-methods/)

##### things to do: 

1. write site update script [link here](http://stackoverflow.com/questions/23087463/batch-script-to-find-and-replace-a-string-in-text-file-within-a-minute-for-files). can start w this: 

```
python -c "with open('%textfile%', 'rw') as f: f.write(f.read().replace('%search%', '%replace%'))"
```

2. compare auto theory building idea to goodman auto hyp gen paper

3. blog post on string interpolation for programming with dplyr?! (see rainin code i think)


5. r paqq for comparing images

6. ...


<br><br><br><br><br><br>


##### add to linxe listte

[blah](https://cartesianfaith.com/2017/06/04/fermi-poker-gambling-for-quants-and-data-scientists/)
[blah](https://github.com/zatonovo/lambda.r)
[blah](http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/)
[blah](https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf)
[blah](http://courses.had.co.nz/11-devtools/)
[blah](https://github.com/gastonstat/tutorial-R-noninteractive/blob/master/01-introduction.Rmd)
[blah](http://had.co.nz/stat405/)

[blah](https://www.quantopian.com/tutorials/getting-started)
[blah](https://www.youtube.com/user/sentdex/videos?live_view=500&sort=dd&view=0&flow=grid)

[blah](https://biologyforfun.wordpress.com/2015/02/26/generating-anova-like-table-from-glmm-using-parametric-bootstrap/)

[useful lme4 notes](http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html)

[more lme4 notes](http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/)

[lme4 convergence warning troubleshooting](http://rpubs.com/bbolker/lme4trouble1)


[mcmcglmm tutorial](https://github.com/tmalsburg/MCMCglmm-intro) (very nice -- revisit)

another twitter/monkeylearn combo r post (similar to my btc one): http://www.masalmon.eu/2017/05/20/billnye/


potentially super useful for debugging, code organization/reorganization, etc.

https://cran.rstudio.com/web/packages/CodeDepends/vignettes/intro.html


to install bioconductor paqqs, e.g.:
  http://bioconductor.org/packages/release/bioc/html/Rgraphviz.html

```
f = "../../../../projjies/sandboxxxe/boosh/permfunc.r"
sc = readScript(f)
g = makeVariableGraph( info = getInputs(sc))
plot(g)

makeCallGraph("package:lefftpack")
gg = makeCallGraph("package:lefftpack")
gg = layoutGraph(gg, layoutType = "circo")
graph.par(list(nodes = list(fontsize=55)))
renderGraph(gg) ## could also call plot directly
plot(gg)
```

bootstrapping regression coefficients example (shd rewrite the func):

```
library('car')
m1 <- lm(Fertility ~ ., swiss)
betahat <- coef(m1)
summary(m1)
betahat.boot <- bootCase(m1, B=99) # 99 bootstrap samples--too small to be useful
summary(betahat.boot)  # default summary
cbind("Bootstrap SD"=apply(betahat.boot, 2, sd),
      t(apply(betahat.boot, 2, function(x) quantile(x, c(.025, .975)))))
```


- write an api for movie scrippse --> http://www.imsdb.com/scripts/Ex-Machina.html
- sjmisc paqq, maybe send to stuieuie --> http://www.strengejacke.de/sjPlot/sjmisc/
- incrementally visualize python execution line by line --> http://pythontutor.com/visualize.html#mode=display
- job board --> https://www.r-users.com/
- new monte carlo paqq --> https://firstdifferences.wordpress.com/2017/06/08/introducing-the-montecarlo-package/
- marginal effects maybe actually useful --> https://www.r-bloggers.com/stata-like-marginal-effects-for-logit-and-probit-models-in-r/
- double pipe logger --> 
- extending logger pipe --> https://cran.r-project.org/web/packages/lumberjack/vignettes/extending.html
- R6 classes --> https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
- more on R6 classes --> https://cran.r-project.org/web/packages/R6/vignettes/Performance.html
- ***simulating imputed values --> https://cran.r-project.org/web/packages/simputation/vignettes/intro.html


