
### game objects --------------------------------------------------------------


## the wheel --------------------------
wheel <- data.frame(
  # a wheel representing the numbers, their physical indices, and their colors
  stringsAsFactors=FALSE, 
  # assign indices starting at 1, clockwise
  wheel_index = seq(len=38),
  # sequence of numbers on the wheel (clockwise)
  number = c("0","28","9","26","30","11","7","20","32",
             "17","5","22","34","15","3","24","36","13","1",
             "00","27","10","25","29","12","8","19","31",
             "18","6","21","33","16","4","23","35","14","2"),
  # the colors corresponding to each wheel number
  color = c("green", rep(c("black","red"), times=9),     # spaces 1-19
            "green", rep(c("red","black"), times=9))     # spaces 20-38
)

## the table --------------------------
table <- ""


### game actions --------------------------------------------------------------






### external sources of info about the game -----------------------------------


########################################################### THE WHEEL #########
# source: http://www.predictem.com/roulette/wheel.php
# image:  img/predictem_roulette-wheel.gif
#
#   The Roulette wheel is made of 38 numbers which include 1-36, 0 and 00.
#
#   To determine the winning number, a croupier/dealer spins the wheel one way
#   and then takes a little white ball and spins it the other way inside the
#   wheel which has a tilted circular track that the ball goes around in.
#
#   Once the ball loses it's momentum, it usually bounces around and then
#   finally comes to rest within one of the "pockets" (numbers).
#   Players are then paid out accordingly.
#
#   Almost all roulette wheels are numbered in the same order as others.
#   This order consists of: (going clockwise)
#
#   Double zero wheel: (American)
#   0-28-9-26-30-11-7-20-32-17-5-22-34-15-3-24-36-13-1-
#   00-27-10-25-29-12-8-19-31-18-6-21-33-16-4-23-35-14-2
#
#   You can view the image below to decipher which numbers are red and
#   which numbers are black. As you can see, the zero and double zero are
#   the only two green numbers on the wheel.




########################################################### THE TABLE #########
# source: http://www.predictem.com/roulette/table.php
# image:  img/predictem_roulette-table.gif
#
#   The roulette table layout is really quite easy to understand.
#   In the United States or American roulette, there are a total of 38 numbers
#   to bet on. These numbers include 1-36 and 0, 00. The difficult decision on
#   a roulette table is choosing which bet or bets to make. Do you go for an
#   outside bet or an inside bet? Well, you can do both if you'd like but for
#   starters, you should know what you're getting yourself into. Let's take a
#   gander at how the roulette table layout is set up.
#
#   First, American roulette tables have eighteen red numbers, eighteen black
#   numbers, and two green numbers (0, and 00). The numbers one through
#   thirty six (1-36) are divided into three separate rows that run the length
#   of the layout. The 0 and 00 numbers are located at the top of the
#   three rows just above the numbers 1, 2 and 3. This entire collection of
#   numbers is considered the "inside" of the table layout thus if a player
#   makes an inside bet he will be wagering in this area.
#
#   The outside bets on a roulette table are any designated "box"
#   or specifically labeled area that is not an inside number or
#   on the inside of the layout. This area of the layout is what some
#   people would consider as the more conservative betting area as the risk
#   involved is not as high as on the inside bets. The inside bets payoffs
#   range from 5-1 all the way up to 35-1 compared to the outside bets
#   where the payoffs range from 1-1 to 2-1.
#
#   Slightly above and at the head of the roulette table layout there will be
#   a roulette wheel which has all of the corresponding numbers as those
#   located on the layout. A dealer will spin the wheel one way and spin a
#   little white ball the opposite way which glides along a grooved track.
#   After the wheel is set in motion, the dealer will hold his hand over the
#   layout and wave it back and forth indicating that no more bets will be
#   accepted. The wheel begins to slow and then the little white ball is
#   randomly jolted from its cruise and finds a numbered divot to rest in.
#   This number is the winner for that spin. Easy money!
#
#   Another part of the roulette layout which has become more prevalent in
#   the last decade or so is the electronic score sign. The score sign is a
#   device that tracks the most recent results at the roulette table.
#   The sign is usually located near eye level close to the roulette wheel.
#   A novice player will not understand the importance of following the table
#   trends but a seasoned player will definitely take note of the mood and
#   tenancy of the numbers on the board. Charting a roulette table is an
#   article in itself and that will be discussed later, but for now simply
#   think of it as looking for a dominant or consistent trend of numbers.
#
#   Roulette can be a more relaxing game if you understand the layout and
#   know your way around the table. It's a game of chance and can be played
#   both aggressively or conservatively. The educated bettor always has a
#   little edge over those who go to the tables blind and uninformed.
#   Learning the layout of a table is the first step to success in any
#   gambling situation. Luck to ya.


############################################################################### 
# possible bets for double-zero wheel: 
# 
#   - straight bet: 35x
#   - split: 17x
#   - corner: 8x
#   - street: 11x (3num)
#   - double street: 5x (6num)
#   - five-num/top-line: 6x (5num)
#   - red/black: 1x
#   - odd/even: 1x
#   - 1-18: 1x
#   - 19-36: 1x
#   - first12: 2x
#   - second12: 2x
#   - third12: 2x
#   - column1: 2x
#   - column2: 2x
#   - column3: 2x



### scratch area --------------------------------------------------------------

# wheel <- data.frame(
#   number = c("00", as.character(0:36)),
#   color  = c(
#     # the double-zero and zero
#     "green", "green",
#     # spaces 1-10
#     rep(c("red","black"), times=5),
#     # spaces 11-19
#     rep(c("black","red"), times=4), "black",
#     # spaces 20-27
#     rep(c("black","red"), times=4),
#     # spaces 28-35
#     rep(c("red","black"), times=4), "red"
#   ),
#   # fill this in later
#   # (start at zero/double zero, index clockwise)
#   wheel_index = "FILL IN LATROWE"
# )

