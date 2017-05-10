

#### playground for nat lang semantics implementations


##### start by building the following:
  1. bare-bones extensional semantics (vocab + example mod)
  2. bare-bones model-theory (generalize model from 1.)
  3. bare-bones syntax (cfg-based)
  4. intensional/relational semantics (parameterized lex + kripke frames)
  5. syntax for gradable expressions (add to cfg)
  6. gradability in the semantics (lexicon + model theory)
  7. syntax generalized to a probabilistic cfg
  8. ...


##### possible uses, once reasonably developed:
  - find collaborators to work on a semantics R package with (pedagogy?);
  - basis of simulations to integrate quant stuff w reggie semantics;
  - playground for testing theories.
  - ...


##### notes:
  - for (e.g.) 'chased', order of args wrong! x shd be do not sb (line87)!
  - all items recompute ITEM at each call
  - need to read on dsl's to figger out best way to encapsulate this all
  - ...