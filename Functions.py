# -*- coding: utf-8 -*-
"""
Created on Thu May 23 11:30:26 2024

@author: Martin
"""



def el_window_routine(el_tracker, EyeLinkCoreGraphicsPsychoPy,
                      monitor_physical_width= 53.0, 
                      monitor_physical_distance=53.0, 
                      foreground_color= (-1, -1, -1),
                      background_color= (1, 1, 1),
                      full_screen= True):

    # opens up a psychopy window and adds eyelink routine    
        
    import pylink
    from psychopy import visual, monitors
    
    
    # Set up a graphics environment for calibration
    #
    # Open a window, be sure to specify monitor parameters
    mon = monitors.Monitor('myMonitor', width=monitor_physical_width, distance= monitor_physical_distance)
    win = visual.Window(fullscr= full_screen,
                        monitor= mon,
                        winType='pyglet',
                        units='pix')
    
    # get the native screen resolution used by PsychoPy
    scn_width, scn_height = win.size
    
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
    genv = EyeLinkCoreGraphicsPsychoPy(el_tracker, win)
    print(genv)  # print out the version number of the CoreGraphics library
    
    # Set background and foreground colors for the calibration target
    # in PsychoPy, (-1, -1, -1)=black, (1, 1, 1)=white, (0, 0, 0)=mid-gray
    genv.setCalibrationColors(foreground_color, background_color)
    
    # Set up the calibration target:
    genv.setTargetType('circle')
    
    # Configure the size of the calibration target (in pixels)
    # this option applies only to "circle" and "spiral" targets
    genv.setTargetSize(24)
    
    # Beeps to play during calibration, validation and drift correction:
    genv.setCalibrationSounds('', '', '')
    
    # Request Pylink to use the PsychoPy window we opened above for calibration
    pylink.openGraphicsEx(genv)
    
    return win