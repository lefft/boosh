### slightly modified source files for interactive use 
from utils import *
from lexica import *
from pragmods import *

### DEMO FROM LEXICA.PY
lexica = Lexica(
        baselexicon={'some': ['w_SOMENOTALL', 'w_ALL'], 'all': ['w_ALL']},
        costs={'some':0.0, 'all':0.0},
        join_closure=True,
        nullsem=True)

lexica.display()



###############################################################################
### DEMO FROM PRAGMODS.PY #####################################################
# 
# Example involving the division of pragmatic labor (marked forms
# express marked meanings; unmarked forms express unmarked
# meanings). This can be captured in the lexical uncertainty
# models but not the fixed-lexicon ones.
# 
# The three non-contradictory propositions:
TT = [1.0, 1.0]
TF = [1.0, 0.0]
FT = [0.0, 1.0]
# 
# Semantics for the null message fixed for all lexica:
nullsem = [1.0, 1.0]
# 
# The nine logically distinct lexica -- message rows, world columns:
lexica = [
    np.array([TT, TT, nullsem]),
    np.array([TT, TF, nullsem]),
    np.array([TT, FT, nullsem]),
    np.array([TF, TT, nullsem]),
    np.array([TF, TF, nullsem]),
    np.array([TF, FT, nullsem]),
    np.array([FT, TT, nullsem]),
    np.array([FT, TF, nullsem]),
    np.array([FT, FT, nullsem])]
# 
# General model with the temperature parameter (lambda) set aggressively:
mod = Pragmod(
  lexica=lexica,
  messages=['normal-message', 'abnormal-message', 'null'], # Messsages and
  costs=np.array([1.0, 2.0, 5.0]),                         # their costs.
  states=['normal-world', 'abnormal-world'],               # World-types and      
  prior=np.array([2.0/3.0, 1.0/3.0]),                      # their prior.
  lexprior=np.repeat(1.0/len(lexica), len(lexica)),        # Flat lex prior.
  temperature=3.0,
  alpha=1.0, 
  beta=1.0) # Relevant only for the anxious experts model.

# Compare the final listeners (display=True for full model output):
# Iteration depth (sort of arbitrary here):
n = 4
# 
# The base model on the first (true) lexicon:
baselangs = mod.run_base_model(lexica[0], n=n, display=False)    

mod.display_listener_matrix(
  baselangs[-1],
  title=" - Base model")

# Basic lexical uncertainty model:
lulangs = mod.run_uncertainty_model(n=n, display=False)

mod.display_listener_matrix(
  lulangs[-1],
  title=" - Lexical uncertainty model")       

# The Smith et al. uncertainty/anxiety listener:
ualangs = mod.run_anxiety_model(n=n, display=False)

mod.display_listener_matrix(
  ualangs[-1],
  title=" - The anxiety/uncertainty model")

# Lexical uncertainty with anxious experts:
expertlangs = mod.run_expertise_model(n=n, display=False)

mod.display_listener_matrix(
  mod.listener_lexical_marginalization(expertlangs[-1]),
  title=" - The anxious experts model")

##################################################
# Streaming lexical uncertainty model:
def lexicon_iterator():
  for x in lexica:
    yield x    

mod = Pragmod(
  lexica=lexicon_iterator,
  messages=['normal-message', 'abnormal-message', 'null'], # Messsages and
  costs=np.array([1.0, 2.0, 5.0]),                         # their costs.
  states=['normal-world', 'abnormal-world'],               # World-types and
  prior=np.array([2.0/3.0, 1.0/3.0]),                      # their prior.
  temperature=3.0)

mod.stream_lexical_uncertainty(n=n)

mod.display_listener_matrix(
  mod.final_listener,
  title=" - Streaming lexical uncertainty model") 








### DEFINED IN UTILS.PY
# 
# - rownorm(mat)
#   """Row normalization of a matrix"""
# 
# - colnorm(mat)
#   """Column normalization of a matrix"""    
# 
# - safelog(vals)
# 
# - display_matrix(mat, rnames=None, cnames=None, title='', digits=4, latex=False)
#   """Utility function for displaying strategies to standard output."""
# 
# - powerset(x, minsize=0, maxsize=None)
# 
# - mse(x, y)
#   """Mean squared error"""
# 
# 


### DEFINED IN LEXICA.PY
# 
# - NULL_MSG = 'NULL'
# 
# - DISJUNCTION_SIGN = ' v '
# 
# - CONJUNCTION_SIGN = ' & '
# 
# - Lexica() class 
#   >> args to init method: 
#     - baselexicon, dict, 
#         Mapping strings to iteraables.
#     - atomic_states=[], list, 
#         Usually the atomic states are the keys of `baselexicon`,
#         but that set can be augmented if one wants states
#         where no message is true (except `nullsem` perhaps).
#     - nullsem=True, 
#         If True, add the null message, true in all states in all lexica.
#     - join_closure=False, 
#         Close the messages and states under disjunction.
#     - block_ineffability=False, 
#         Block states without true messages; relevant only if `nullsem=False`.
#     - costs=defaultdict(float), 
#         `costs.keys()` must contain `baselexicon.keys()`
#     - disjunction_cost=.01, 
#         Cost of a disjunction.
#     - nullcost=5, 
#         Should probably be higher than regular messages' costs.
#     - unknown_word=None
#         A message constrained to have a singleton meaning in all lexica.
# 
#   >> attributes (init args become attrs): 
#     - messages (list)
#         The keys of `baselexicon`.
#     - atomic_states 
#         This is the union of `atomic_states` with all the elements
#         of the list valued `self.baselexicon`.
#     - self.states : list
#         Same as self.atomic_states` at first, but possibly
#         expanded by closures later.
#     - lexica : list
#         Created by `self.get_lexica`.


### DEFINED IN PRAGMODS.PY
# 
# - Pragmod() class 
#   >> args to init method: 
#     - name="",
#         Optional informal name for the model.
#     - lexica=None,
#         list of np.array, Dimension m x n.
#     - baselexicon=None,
#         np.array  The starting point for the space of lexica.
#     - messages=None, 
#         iterable, length m  
#     - states=None, 
#         iterable, length n          
#     - costs=None, 
#         np.array, length m, w float vals. if None, then 0 costs assumed 
#         for all messages except nullmsg, if there is one. if there is, it 
#         is given its own cost as specified by nullcost 
#     - prior=None, 
#         np.array, Length n, with float values summing to 1.0. If `None`, 
#         then this becomes an even distribution over states.
#     - lexprior=None,
#         np.array, length len(self.lexica) w float vals summing to 1. 
#         if no lexicon prior is given, but we do know the number of lexica 
#         (lexcount), then we define a flat prior over lexica. 
#         If no `lexcount` is given, we lead this undefined and the 
#         lexicon prior is implicitly flat.
#     - lexcount=None, 
#         int, Number of lexica if known ahead of time.
#     - temperature=1.0,
#         float, usually \lambda but lambda is a python builtin, should be >0
#     - alpha=1.0,
#         float, speaker value for the world state 
#     - beta=1.0,
#         float, Speaker value for the lexicon.
#     - nullmsg=True,
#         bool, whether to assume final message is null 
#     - nullcost=5.0
#         float, cost for nullmsg if exists; shd be positive 
#   
#   >> attributes
#     - all init args are attrs
#     - self.final_listener : initialized as an all 0s matrix
#     - self.final_speaker : initialized as `None`
#     - Both of above two are filled in my model methods, allowing for
#       easier access to the final agents.        











