# -*- coding: utf-8 -*-
"""
Created on Fri May 31 10:44:30 2024

@author: Martin
"""


from psychopy import visual, core, monitors, gui, event

full_screen= True # open in full screen mode  
monitor_physical_width= 53.0 # width in cm
monitor_physical_distance= 70 # distance in cm
foreground_color = (-1, -1, -1) # black
background_color = (1, 1, 1) # white  

mon = monitors.Monitor('myMonitor', width=monitor_physical_width,
                       distance= monitor_physical_distance)
win = visual.Window(size=(1920, 1080), fullscr= full_screen,   
                    monitor= mon,
                    units='pix')

# get the native screen resolution used by PsychoPy
scn_width, scn_height = win.size

# core.wait(3)

win.winHandle.minimize() # minimise the PsychoPy window
win.winHandle.set_fullscreen(False) # disable fullscreen
win.flip() # redraw the (minimised) window


core.wait(3)

# maximize the PsychoPy window
win.winHandle.maximize()
win.winHandle.set_fullscreen(True) 
win.winHandle.activate()


# prepare the target dot and the central fixation cross
target = visual.GratingStim(win, tex=None, mask='circle', size=50,
                            pos=(scn_width/2, scn_height/2), color=(1, -1, -1))
fix_cross = visual.TextStim(win, '+', height=50, color=(-1, 1, -1))

fix_cross.draw()
win.flip()


core.wait(3)
win.close()




