# see here (philip maymin comments at bottom!)
#   http://www.decisionsciencenews.com/2017/06/19/counterintuitive-problem-everyone-room-keeps-giving-dollars-random-others-youll-never-guess-happens-next/

# original motivation/problem is from: 
#   https://quomodocumque.wordpress.com/2017/06/27/when-random-people-give-money-to-random-other-people/

# source of this code is: 
#   https://lucasvw.github.io/main/2017/06/22/money.html

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation

amount_of_people = 100

n = amount_of_people - 1
p = 1 / n
start_money = 100

bank = start_money*np.ones([amount_of_people])

range = np.arange(amount_of_people)

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

bar = plt.bar(range,bank)

def init():
    return bar

def animate(i):
    global bank, ax
    people_without_money = sum(bank==0)
    smpl_w_money = np.random.binomial(amount_of_people - 1 - people_without_money, p, amount_of_people)
    smpl_wo_money = np.random.binomial(amount_of_people - people_without_money, p, amount_of_people)
    smpl_w_money[bank == 0] = 0
    smpl_wo_money[bank > 0] = 0
    sample = smpl_w_money + smpl_wo_money
    bank = bank + sample - 1*(bank > 0)
    bank = np.sort(bank)
    for rect, y in zip(bar, bank):
        rect.set_height(y)
    ax.set_ylim(0,max(bank))
    ax.set_title(i)
    #print(sum(bank)/amount_of_people)

anim = animation.FuncAnimation(fig, animate, init_func=init,
                               frames=1500, interval=1, blit=False)
plt.show()