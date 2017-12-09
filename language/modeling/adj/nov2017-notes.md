###### goals
  - define a system that replicates fig5 from lg15 
  - actually understand how it works and why 
  - understand exactly where and why mh-mcmc is necessary 
  - write code that is portable to the degree possible 

###### main components of the system 
  - qud's (e.g. how tall is x?) 
  - utterances (answers to qud, e.g. x is tall.) 
  - costs 
  - informativity 
  - 

###### probs we want to compute 
  - prob of th given that ht for x and x is tall 
  - prob that x is tall given ht 
  - prob that x is tall given th 
  - prob that x's height is ht for any ht 
  - prob that x's height is ht given that x is tall 

###### conversational agents
  - LL -- conditions on truth of utterance 
  - SP -- computes prob of u given states of world 
  - PL -- derives interp thru SP and LL   [REVISE]

###### facts to derive 
  - if th is low, 'x is tall' is not very informative 
  - if th is high, 'x is tall' is very informative 




want to estimate `theta`: 

prob(th | 'tall', 70) = 
				                prob('tall', 70 | theta) * prob(th)
				                -----------------------------------
				                           prob('tall', 70)














#  In the  rst step in the interpretation process, R conditions their beliefs on the truth of Sarah ate some of the cookies; that is, they assign the probability 0 to the sets of the worlds in which Sarah eats no cookies, and then normalize the prior probability distribution over the worlds in which at least some cookies are eaten.  is interpretation process, which Lassiter and Goodman call the literal listener, can be written more formally as in (14), where A is a set of possible worlds and u is an utterance/message.
# prob_l0( A | u ) = prob_l0( A | u==TRUE)

# In their  nal calculation, R takes into account how exactly they believe S chooses the message. In particular, a crucial feature of this framework is that R has the belief that the speaker chooses the optimal message based on 1) the conditionalized prior PL0 , 2) informativity, and 3) message costs. For the full formal details of how speaker strategies (notated PS1 (·)) are calculated, see (Frank and Goodman, 2012, among others); however, we can describe them informally as follows:

# 1.  The conditionalized prior ensures that R believes that S would not say anything false (i.e. in this example, it assigns a zero probability to worlds in which Sarah eats no cookies).
# 2.  The informativity constraint ensures that R thinks S’s type is the strongest meaning possible; that is, here is where the preference for the most informative interpretation is encoded. We can observe a similarity because the maximization of informativity in Bayesian pragmatics and the wording of IE: “Maximize the contribution of the conventional meanings of the elements of a sentence to the computation of its truth conditions.”
# 3. Finally, the costs constraint serves to create a dispreference for the use of certain kinds of messages. For example, we might assume that Sarah ate some but not all of the cookies is more costly from a processing and/or production point of view because it is longer than an expression like Sarah ate some of the cookies.

# With optimized speaker strategies in hand, R creates their posterior beliefs based on their prior beliefs and their beliefs about the strategy that the speaker is using. More formally, R’s posterior beliefs are cal- culated as in (15) (notated PL1 , which Goodman and Lassiter call the pragmatic listener).

# prob_l1( A | u ) = 
#   prob_s1( u | A ) * prob_l1( A ) 
#   ------------------------------
#   sum_A': prob_s1( u | A' ) * prob_l1( A' )

# The upshot of all of this is that, since R believes the speaker to always say the most informative thing (modulo costs), in this example as in Grice (1975), they reason that if Sarah ate all of the cookies were true, then S would have picked this message. Since they did not, it is highly likely that it is false; therefore, in most cases, R ends up assigning a very high probability to the set of possible worlds in which Sarah eats some but not all of the cookies.










# step 1: 
#   - specify priors over states (x's height, th??!)
plot(spaces$ht, priors$ht, col="blue"); points(spaces$ht, priors$th, col="red")
# 
# step 2: 
#   - update priors by conditioning on truth of u 
#   - prob_l0(A | u, V) = prob_l0(A | u^V = true)
#   - i.e. condition on thetas that make u true 
#   - (assign 0 to u-false worlds, renormalize)

states_utrue <- states %>% filter(utt=="tall", value==TRUE)
utrue_hts <- states_utrue$ht %>% unique()
priors$ht[as.character(utrue_hts)]
#  
# step 3: 
#   - assume speaker chooses maximally informative u
#   - ensure that cost is used in this calculation 
# 
# step 4: 
#   - use above + bayes thm to get l1 posterior 



# possible states: 
#   - set of height, theta pairs 
# 
# utterance prior over states: 
#   - prob dist across utterances 



# NOTE: PEEP PACKAGE utility::


