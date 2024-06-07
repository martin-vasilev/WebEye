# -*- coding: utf-8 -*-
"""
Created on Fri May 10 12:58:33 2024

@author: Martin R. Vasilev
"""

from __future__ import division
from __future__ import print_function

#import platform
import sys
import pygame
#import random
import time
from pygame.locals import *
from CalibrationGraphicsPygame import CalibrationGraphics
from string import ascii_letters, digits
import pylink
import datetime
import os


def show_message(message, fg_color, bg_color):
    """ show messages on the screen

    message: The message you would like to show on the screen
    fg_color/bg_color: color for the texts and the background screen
    """

    # clear the screen and blit the texts
    win_surf = pygame.display.get_surface()
    win_surf.fill(bg_color)

    scn_w, scn_h = win_surf.get_size()
    message_fnt = pygame.font.SysFont('Arial', 32)
    msgs = message.split('\n')
    for i in range(len(msgs)):
        message_surf = message_fnt.render(msgs[i], True, fg_color)
        w, h = message_surf.get_size()
        msg_y = scn_h / 2 + h / 2 * 2.5 * (i - len(msgs) / 2.0)
        win_surf.blit(message_surf, (int(scn_w / 2 - w / 2), int(msg_y)))

    pygame.display.flip()


def wait_key(key_list, duration=sys.maxsize):
    """ detect and return a keypress, terminate the task if ESCAPE is pressed

    parameters:
    key_list: allowable keys (pygame key constants, e.g., [K_a, K_ESCAPE]
    duration: the maximum time allowed to issue a response (in ms)
              wait for response 'indefinitely' (with sys.maxsize)
    """

    got_key = False
    # clear all cached events if there are any
    pygame.event.clear()
    t_start = pygame.time.get_ticks()
    resp = [None, t_start, -1]

    while not got_key:
        # check for time out
        if (pygame.time.get_ticks() - t_start) > duration:
            break

        # check key presses
        for ev in pygame.event.get():
            if ev.type == KEYDOWN:
                if ev.key in key_list:
                    resp = [pygame.key.name(ev.key),
                            t_start,
                            pygame.time.get_ticks()]
                    got_key = True

            if (ev.type == KEYDOWN) and (ev.key == K_c):
                if ev.mod in [KMOD_LCTRL, KMOD_RCTRL, 4160, 4224]:
                    terminate_task()

    # clear the screen following each keyboard response
    win_surf = pygame.display.get_surface()
    win_surf.fill(genv.getBackgroundColor())
    pygame.display.flip()

    return resp


def abort_trial():
    """Ends recording

    We add 100 msec to catch final events
    """

    # get the currently active tracker object (connection)
    el_tracker = pylink.getEYELINK()

    # Stop recording
    if el_tracker.isRecording():
        # add 100 ms to catch final trial events
        pylink.pumpDelay(100)
        el_tracker.stopRecording()

    # clear the screen
  #  surf = pygame.display.get_surface()
  #  surf.fill((128, 128, 128))
  #  pygame.display.flip()
    # Send a message to clear the Data Viewer screen
    el_tracker.sendMessage('!V CLEAR 128 128 128')

    # send a message to mark trial end
    el_tracker.sendMessage('TRIAL_RESULT %d' % pylink.TRIAL_ERROR)

    return pylink.TRIAL_ERROR



def terminate_task():
    """ Terminate the task gracefully and retrieve the EDF data file

    file_to_retrieve: The EDF on the Host that we would like to download
    win: the current window used by the experimental script
    """

    # disconnect from the tracker if there is an active connection
    el_tracker = pylink.getEYELINK()

    if el_tracker.isConnected():
        # Terminate the current trial first if the task terminated prematurely
        error = el_tracker.isRecording()
        if error == pylink.TRIAL_OK:
            abort_trial()

        # Put tracker in Offline mode
        el_tracker.setOfflineMode()

        # Clear the Host PC screen and wait for 500 ms
        el_tracker.sendCommand('clear_screen 0')
        pylink.msecDelay(500)

        # Close the edf data file on the Host
        el_tracker.closeDataFile()

        # Show a file transfer message on the screen
        #msg = 'EDF data is transferring from EyeLink Host PC...'
        #show_message(msg, (0, 0, 0), (128, 128, 128))

        # Download the EDF data file from the Host PC to a local data folder
        # parameters: source_file_on_the_host, destination_file_on_local_drive
        local_edf = os.path.join(session_folder, session_identifier + '.EDF')
        try:
            el_tracker.receiveDataFile(edf_file, local_edf)
        except RuntimeError as error:
            print('ERROR:', error)

        # Close the link to the tracker.
        el_tracker.close()

    # quit pygame and python
    pygame.quit()
    sys.exit()




# initialize pygame
pygame.init()


# SETTINGS:
dummy_mode= False # run Eyelink in dummy mode    
full_screen= True # open in full screen mode  
monitor_physical_width= 53.0 # width in cm
monitor_physical_distance= 70 # distance in cm
foreground_color = (-1, -1, -1) # black
background_color = (1, 1, 1) # white  



#################################
###   1) OPEN ONLINE STUDY:     #
#################################
#driver = webdriver.Chrome() # using Edge as Chrome doesn't work with the website on my laptop..
#driver.maximize_window() # go full screen
#driver.get(study_url) # open url window



#################################
###   2) SET UP EDF FILENAME:   #
#################################
    
edf_fname = 'TEST'

# Prompt user to specify an EDF data filename
# before we open a fullscreen window
while True:
    # use "raw_input" to get user input if running with Python 2.x
    try:
        input = raw_input
    except NameError:
        pass
    prompt = '\nSpecify an EDF filename\n' + \
        'Filename must not exceed eight alphanumeric characters.\n' + \
        'ONLY letters, numbers and underscore are allowed.\n\n--> '
    edf_fname = input(prompt)
    # strip trailing characters, ignore the '.edf' extension
    edf_fname = edf_fname.rstrip().split('.')[0]

    # check if the filename is valid (length <= 8 & no special char)
    allowed_char = ascii_letters + digits + '_'
    if not all([c in allowed_char for c in edf_fname]):
        print('ERROR: Invalid EDF filename')
    elif len(edf_fname) > 8:
        print('ERROR: EDF filename should not exceed 8 characters')
    else:
        break


# Set up a folder to store the EDF data files and the associated resources
# e.g., files defining the interest areas used in each trial
results_folder = 'results'
if not os.path.exists(results_folder):
    os.makedirs(results_folder)

# We download EDF data file from the EyeLink Host PC to the local hard
# drive at the end of each testing session, here we rename the EDF to
# include session start date/time
time_str = time.strftime("_%Y_%m_%d_%H_%M", time.localtime())
session_identifier = edf_fname + time_str

# create a folder for the current testing session in the "results" folder
session_folder = os.path.join(results_folder, session_identifier)
if not os.path.exists(session_folder):
    os.makedirs(session_folder)

   
    
#######################################
###   3) SET UP TRACKER CONNECTION:   #
#######################################

# Connect to the EyeLink Host PC
#
# The Host IP address, by default, is "100.1.1.1".
# the "el_tracker" objected created here can be accessed through the Pylink
# Set the Host PC address to "None" (without quotes) to run the script
# in "Dummy Mode"

if dummy_mode:
    el_tracker = pylink.EyeLink(None)
else:
    try:
        el_tracker = pylink.EyeLink("100.1.1.1")
    except RuntimeError as error:
        print('ERROR:', error)
        core.quit()
        sys.exit()

# Step 2: Open an EDF data file on the Host PC
edf_file = edf_fname + ".EDF"
try:
    el_tracker.openDataFile(edf_file)
except RuntimeError as err:
    print('ERROR:', err)
    # close the link if we have one open
    if el_tracker.isConnected():
        el_tracker.close()
    core.quit()
    sys.exit()

# Add a header text to the EDF file to identify the current experiment name
# This is OPTIONAL. If your text starts with "RECORDED BY " it will be
# available in DataViewer's Inspector window by clicking
# the EDF session node in the top panel and looking for the "Recorded By:"
# field in the bottom panel of the Inspector.
preamble_text = 'RECORDED BY %s' % os.path.basename(__file__)
el_tracker.sendCommand("add_file_preamble_text '%s'" % preamble_text)


#######################################
###   4) CONFIGURE TRACKER:           #
#######################################

# Put the tracker in offline mode before we change tracking parameters
el_tracker.setOfflineMode()

# Get the software version:  1-EyeLink I, 2-EyeLink II, 3/4-EyeLink 1000,
# 5-EyeLink 1000 Plus, 6-Portable DUO
eyelink_ver = 0  # set version to 0, in case running in Dummy mode
if not dummy_mode:
    vstr = el_tracker.getTrackerVersionString()
    eyelink_ver = int(vstr.split()[-1].split('.')[0])
    # print out some version info in the shell
    print('Running experiment on %s, version %d' % (vstr, eyelink_ver))

# File and Link data control
# what eye events to save in the EDF file, include everything by default
file_event_flags = 'LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT'
# what eye events to make available over the link, include everything by default
link_event_flags = 'LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON,FIXUPDATE,INPUT'
# what sample data to save in the EDF data file and to make available
# over the link, include the 'HTARGET' flag to save head target sticker
# data for supported eye trackers
if eyelink_ver > 3:
    file_sample_flags = 'LEFT,RIGHT,GAZE,HREF,RAW,AREA,HTARGET,GAZERES,BUTTON,STATUS,INPUT'
    link_sample_flags = 'LEFT,RIGHT,GAZE,GAZERES,AREA,HTARGET,STATUS,INPUT'
else:
    file_sample_flags = 'LEFT,RIGHT,GAZE,HREF,RAW,AREA,GAZERES,BUTTON,STATUS,INPUT'
    link_sample_flags = 'LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,INPUT'
el_tracker.sendCommand("file_event_filter = %s" % file_event_flags)
el_tracker.sendCommand("file_sample_data = %s" % file_sample_flags)
el_tracker.sendCommand("link_event_filter = %s" % link_event_flags)
el_tracker.sendCommand("link_sample_data = %s" % link_sample_flags)

# Optional tracking parameters
# Sample rate, 250, 500, 1000, or 2000, check your tracker specification
# if eyelink_ver > 2:
#     el_tracker.sendCommand("sample_rate 1000")
# Choose a calibration type, H3, HV3, HV5, HV13 (HV = horizontal/vertical),
el_tracker.sendCommand("calibration_type = HV9")
# Set a gamepad button to accept calibration/drift check target
# You need a supported gamepad/button box that is connected to the Host PC
el_tracker.sendCommand("button_function 5 'accept_target_fixation'")


#######################################
###   5) STUDY AND WINDOW SET-UP:     #
#######################################

# Set up a graphics environment for calibration
#
# Open a window, be sure to specify monitor parameters

# open a Pygame window
win=None
if full_screen:
    win = pygame.display.set_mode((0, 0), FULLSCREEN | DOUBLEBUF)
else:
    win = pygame.display.set_mode((0, 0), 0)
    
scn_width, scn_height = win.get_size()
pygame.mouse.set_visible(False)  # hide mouse cursor

# Pass the display pixel coordinates (left, top, right, bottom) to the tracker
# see the EyeLink Installation Guide, "Customizing Screen Settings"
el_coords = "screen_pixel_coords = 0 0 %d %d" % (scn_width - 1, scn_height - 1)
el_tracker.sendCommand(el_coords)

# Write a DISPLAY_COORDS message to the EDF file
# Data Viewer needs this piece of info for proper visualization, see Data
# Viewer User Manual, "Protocol for EyeLink Data to Viewer Integration"
dv_coords = "DISPLAY_COORDS  0 0 %d %d" % (scn_width - 1, scn_height - 1)
el_tracker.sendMessage(dv_coords)

# Configure a graphics environment (genv) for tracker calibration
genv = CalibrationGraphics(el_tracker, win)

# Set background and foreground colors
# parameters: foreground_color, background_color
foreground_color = (0, 0, 0)
background_color = (128, 128, 128)
genv.setCalibrationColors(foreground_color, background_color)



# Set up the calibration target
#
# The target could be a "circle" (default), a "picture", a "movie" clip,
# or a rotating "spiral". To configure the type of calibration target, set
# genv.setTargetType to "circle", "picture", "movie", or "spiral", e.g.,
# genv.setTargetType('picture')
#
# Use gen.setPictureTarget() to set a "picture" target
# genv.setPictureTarget(os.path.join('images', 'fixTarget.bmp'))
#
# Use genv.setMovieTarget() to set a "movie" target
# genv.setMovieTarget(os.path.join('videos', 'calibVid.mov'))

# Use the default calibration target ('circle')
genv.setTargetType('circle')

# Configure the size of the calibration target (in pixels)
# this option applies only to "circle" and "spiral" targets
genv.setTargetSize(24)

# Beeps to play during calibration, validation and drift correction
# parameters: target, good, error
#     target -- sound to play when target moves
#     good -- sound to play on successful operation
#     error -- sound to play on failure or interruption
# Each parameter could be ''--default sound, 'off'--no sound, or a wav file
genv.setCalibrationSounds('', '', '')


# Request Pylink to use the PsychoPy window we opened above for calibration
pylink.openGraphicsEx(genv)


# Show the task instructions
task_msg = 'In the task, you may press D to call up calligrarion\n' + \
           'and C to call up calibration.' + \
    '\nPress Ctrl-Q to if you need to quit the task and stop recording.\n'
if dummy_mode:
    task_msg = task_msg + '\nNow, press ENTER to start the task'
else:
    task_msg = task_msg + '\nNow, press ENTER to calibrate tracker'

# Pygame bug warning
pygame_warning = '\n\nDue to a bug in Pygame 2, the window may have lost' + \
                 '\nfocus and stopped accepting keyboard inputs.' + \
                 '\nClicking the mouse helps get around this issue.'
if pygame.__version__.split('.')[0] == '2':
    task_msg = task_msg + pygame_warning

show_message(task_msg, (0, 0, 0), (128, 128, 128))
wait_key([K_RETURN])


## calibrate:
try:
    el_tracker.doTrackerSetup()
except RuntimeError as err:
    print('ERROR:', err)
    el_tracker.exitCalibration()    
    
pygame.display.quit()
pylink.closeGraphics()

# start recording:
    
try:
    el_tracker.startRecording(1, 1, 1, 1)
except RuntimeError as error:
    print("ERROR:", error)
    
# Allocate some time for the tracker to cache some samples
#pylink.pumpDelay(100)

unix_timestamp = int(datetime.datetime.timestamp(datetime.datetime.now())*1000)
el_tracker.sendMessage('UNIX '+ str(unix_timestamp))    
    
terminate_task()

# stop recording:
# try:
#     el_tracker.stopRecording()
# except RuntimeError as error:
#     print("ERROR:", error)

# Close the edf data file on the Host
#el_tracker.closeDataFile()

# Download the EDF data file from the Host PC to a local data folder
# parameters: source_file_on_the_host, destination_file_on_local_drive
#local_edf = os.path.join(session_folder, session_identifier + '.EDF')
# try:
#     el_tracker.receiveDataFile(edf_file, local_edf)
# except RuntimeError as error:
#     print('ERROR:', error)

#el_tracker.close() 






#terminate_task()


# # Put tracker in Offline mode
# el_tracker.setOfflineMode()


# pylink.msecDelay(500)

# # Close the edf data file on the Host
# el_tracker.closeDataFile()

# # Download the EDF data file from the Host PC to a local data folder
# # parameters: source_file_on_the_host, destination_file_on_local_drive
# local_edf = os.path.join(session_folder, session_identifier + '.EDF')
# try:
#     el_tracker.receiveDataFile(edf_file, local_edf)
# except RuntimeError as error:
#     print('ERROR:', error)

# # Close the link to the tracker.
# el_tracker.close()

# # win= el_window_routine(el_tracker, EyeLinkCoreGraphicsPsychoPy)

# # core.wait(3)
# # win.close()





