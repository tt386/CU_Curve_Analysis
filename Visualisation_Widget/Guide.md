# Visualisation Tool

A simple widget for trying to find the co-moving reference frame speed for travelling wave solution to different models of cardiomyocyte excitation. This widget features the ability to save and load previous work, and has limited capabilities to manipulate the curve to help get rid of double peaks. There is also a very simple algorithm for finding the wavespeed of the travelling wave results.

## Features

### File

#### Save
Saves the current state of the system:
- The u and w curves
- The value of the 'co-moving' frame, c
- The value of the input current, p
- The time
- The model being employed

You can name it anything anywhere, as an npz file

#### Load
Loads the data described above, effectively setting the state to the saved state.

Automatically pauses for analysis.

### 'Space Selector'

#### Space selector slider
This changes the left and right parts of the shaded rectangle, which dictates the boundary of the following effects:

##### Wipe Outside
The u and w curves are set to 0 outside the selected region - very useful for getting rid of duplicated travelling waves

##### Perturb inside
Set the u curve to the value written in the 'Perturb' text box below.

### Reset
A button which resets all the options and curves to the default option, to help escape scenarios where everything gets broken.

### Speed slider
Vary the speed of the reference frame - when the travelling wave is stationary, this speed corresponds to the speed of the wave.

### Input slider
The input voltage. 

### 'Find Speed' checkbox
Enables an algorithm which aims to find the reference frame speed which corresponds to the wave-speed. This is done by looking at the peak of the w curve, and adjusting the speed based on if the peak goes left or right. We keep track of previously explored speed values, and lower the speed step size accordingly so we hone in on a more precise result. Visibly, you should see that the wave becomes stationary.

### Radio Box
Choose the model being employed:
- FHN: the Fitzhugh Nagumo Model
- Karma Step: the Karma model in which the step function is being used.
- Karma Ramp: the Karma model in which the ramp function is being used.
- Morris-LEcar: The Morris-Lecar model.

The parameters for each are hardcoded in the script.



