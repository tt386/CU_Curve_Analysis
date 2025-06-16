import tkinter as tk
from tkinter import ttk

from tkinter import filedialog,Menu

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

from matplotlib.widgets import Slider
from matplotlib.widgets import Button
from matplotlib.widgets import TextBox
from matplotlib.widgets import RangeSlider
from matplotlib.widgets import CheckButtons
from matplotlib.widgets import RadioButtons
import matplotlib.patches as patches
from scipy.fft import fft, ifft
import matplotlib.animation as animation
import copy


PLAY = True



root = tk.Tk()
root.title("Cardiomyocyte Excitation Models")







# Signal points and x-space
N = 2048
x_max = N / 2
x = np.linspace(-x_max / 2, x_max / 2, N)
k = 2 * np.pi * np.fft.fftfreq(N, x_max / N)

I0 = 0.0#-0.5
I = np.ones(len(x)) * (I0)
Ihat = fft(I)
dt = 0.1


ones = np.ones(len(x))

c_init = 1
def Initialise(event):
    # Initial conditions
    u = np.zeros(len(x))
    u[x < 100] = 1
    u[x < -100] = 0
    v = np.zeros(len(x))

    state["u"] = u
    state["v"] = v
    state["t"] = 0
    state["c"] = c_init

    """
    I = np.ones(len(x)) * (I0)
    Ihat = fft(I)
    """
    state["I"] = I0

    try:
        c_slider.set_val(c_init)
        I_slider.set_val(I0)
    except:
        print("First Initialisation")



##############################################################################
#Functions
##############################################################################
def FHN(u,w,s,I):
    d = 5
    a = 0.1
    g = 1
    e = 0.01

    #################################
    # Nonlinear term                #
    def f(y,a):                     #
        return -y**3 + u**2 * (1+a) #
    #################################

    Ihat = fft(ones*I)


    #Fourier transform to u
    uhat = np.fft.fft(u)
    what = np.fft.fft(w)

    #Fourier transoform the function
    fu = np.fft.fft(f(u,a))


    #Derive new uhat and what
    alpha = 1/(1 + d*k**2*dt - s*1j*dt*k + a*dt)
    beta = 1/(1 + g*e*dt - s*1j*dt*k)
    
    uhat_new = alpha/(1 + alpha*dt**2*beta*e) * (uhat + dt * (fu-beta*what 
        + Ihat))
    
    what_new = beta * (what + e*dt*uhat_new)
    
    #Translate back to u and w
    u_new = np.real(ifft(uhat_new))
    w_new = np.real(ifft(what_new))

    
    return u_new, w_new


##############################################################################
##############################################################################

def Karma_Ramp(u,v,c,I):


    # Parameters
    us = 1.5
    D = 1
    d = 0.25
    M = 4
    nb = 0.5
    e = 0.01


    #####################################################
    #Nonlinear terms                                    #
    def f(u, v):                                        #
        return 2 * (us - v ** M) * (u ** 2 - d * u ** 3)#
                                                        #
    def H(u):                                           #
        return u / (1 + np.exp(-2 * u / 0.001))         #
    #####################################################

    Ihat = fft(ones*I)

    uhat = fft(u)
    vhat = fft(v)
    fu = fft(f(u, v))
    HHat = fft(H(u - 1))

    
    alpha = 1 / (1 - dt * (c * 1j * k - D * k ** 2 - 1))
    beta = 1 / (1 + e * dt - c * 1j * k * dt)
    """
    alpha = 1 / (1 - dt * (-c * 1j * k - D * k ** 2 - 1))
    beta = 1 / (1 + e * dt + c * 1j * k * dt)
    """

    uhat_new = alpha * (uhat + dt * (fu + Ihat))
    vhat_new = beta * (vhat + dt * e * HHat / nb)

    u_new = np.real(ifft(uhat_new))
    v_new = np.real(ifft(vhat_new))


    return u_new, v_new


def Karma_Step(u,v,c,I):

    # Parameters
    us = 1.5
    D = 1
    d = 0.25
    M = 4
    nb = 0.5
    e = 0.01


    #####################################################
    #Nonlinear terms                                    #
    def f(u, v):                                        #
        return 2 * (us - v ** M) * (u ** 2 - d * u ** 3)#
                                                        #
    def H(u):                                           #
        return 1 / (1 + np.exp(-2 * u / 0.001))         #
    #####################################################

    Ihat = fft(ones*I)

    uhat = fft(u)
    vhat = fft(v)
    fu = fft(f(u, v))
    HHat = fft(H(u - 1))


    alpha = 1 / (1 - dt * (c * 1j * k - D * k ** 2 - 1))
    beta = 1 / (1 + e * dt - c * 1j * k * dt)
    """
    alpha = 1 / (1 - dt * (-c * 1j * k - D * k ** 2 - 1))
    beta = 1 / (1 + e * dt + c * 1j * k * dt)
    """

    uhat_new = alpha * (uhat + dt * (fu + Ihat))
    vhat_new = beta * (vhat + dt * e * HHat / nb)

    u_new = np.real(ifft(uhat_new))
    v_new = np.real(ifft(vhat_new))


    return u_new, v_new


##############################################################################
##############################################################################

def Morris_Lecar(u,v,c,I):

    V,N,s = u,v,c 

    V1 = -1.2
    V2 = 18
    V3 = 0  #VRIED PARAMETER: 0, -13, -21
    V4 = 10#30
    phi = 0.15#0.04
    Vk = -100#-84
    VL = -70#-60
    VCa = 50#120
    gk = 20#8
    gL = 2
    gCa = 20#4.4
    C = 2#20
    D = 1

    #############################################    
    #Model Functions                            #
    def MSS(V):                                 #
        return 1/2 * (1 + np.tanh((V-V1)/V2))   #
                                                #
    def NSS(V):                                 #
        return 0.5 * (1 + np.tanh((V-V3)/V4))   #
                                                #
    def tN(V):                                  #
        return 1/(phi * np.cosh((V-V3)/(2*V4))) #
    #############################################

    #####################################
    #Nonlinear terms                    #
    def f(V,N):                         #
        return gCa * MSS(V)*(V-VCa)     #
                                        #
    def g(V,N):                         #
        return gk * N * V               #
                                        #
    def n(V,N):                         #
        return (NSS(V) - N)/tN(V)       #
                                        #
    #####################################


    Ihat = np.fft.fft(ones*I)

    Vhat = np.fft.fft(V)
    Nhat = np.fft.fft(N)

    #Fourier transoform the function
    fhat = np.fft.fft(f(V,N))
    ghat = np.fft.fft(g(V,N))
    nhat = np.fft.fft(n(V,N))


    #Derive new uhat and what
    #alpha = 1/(1 - s*1j*k*dt + D*dt*k**2 + np.fft.fft(np.ones(len(x)))*gL*dt 
    #)#1/(1 - dt*(c*1j*k - D*k**2-1))#1/(1 + d*k**2*dt - s*1j*dt*k + a*dt)
    alpha = 1/(1 - s*1j*k*dt/C + D*dt*k**2/C + gL*dt/C )
    beta = 1/(1 - s*1j*k*dt)#1/(1 + e*dt-c*1j*k*dt)#1/(1 + g*e*dt - s*1j*dt*k)
    
    Nhat_new = beta * (Nhat + dt*nhat) 
    
    Vhat_new = alpha * (Vhat + dt/C*(Ihat + np.fft.fft(np.ones(len(x))*gL*VL)
        - fhat - ghat + gk*Vk*Nhat_new))


    V_new = np.real(ifft(Vhat_new))
    N_new = np.real(ifft(Nhat_new))

    return V_new, N_new


##############################################################################
##############################################################################

model_map = {
        "FHN":FHN,
        "Karma Step":Karma_Step,
        "Karma Ramp":Karma_Ramp,
        "Morris-Lecar":Morris_Lecar}

model_func = model_map[tuple(model_map.keys())[0]]



##############################################################################
# Initial Plotting
##############################################################################
# Animation state
state = {'t': 0, 'c': c_init}

Initialise(True)

u = state['u']
v = state['v']

# Plotting setup
fig, ax = plt.subplots()
fig.canvas.manager.set_window_title('Cardiomyocyte Excitation Models')
plt.subplots_adjust(bottom=0.5,right=0.8)
line_u, = ax.plot(x, u, label='u')
line_v, = ax.plot(x, v, label='w')
ax.legend()
ax.grid(True)
ax.set_ylim([-1, 1])
ax.set_xlim([x.min(),x.max()])
title = ax.set_title("t=0.00, c=0.00000")


canvas = FigureCanvasTkAgg(fig, master=root)
canvas_widget = canvas.get_tk_widget()
#canvas_widget.pack(side=tk.TOP, fill=tk.BOTH, expand=1)
canvas_widget.grid(row=0, column=0, columnspan=4)

##############################################################################
##############################################################################











##############################################################################
# Slider setup: change the speed
##############################################################################
ax_slider = plt.axes([0.1, 0.2, 0.8, 0.03])
c_slider = Slider(ax_slider, 'Speed', -3.0, 10.0, valinit=c_init)


##############################################################################
# slider setup: change the current
##############################################################################
ax_Islider = ax_slider = plt.axes([0.1, 0.1, 0.8, 0.03])
I_slider = Slider(ax_Islider, 'Input', -10.0, 10.0, valinit=I0)


##############################################################################
# Button setup: reset
##############################################################################

button_ax = plt.axes([0.1, 0.3, 0.2, 0.04])  # [left, bottom, width, height]
reset_button = Button(button_ax, 'Reset')
reset_button.on_clicked(Initialise)








##############################################################################
#Button setup: wipes region outside of a target
##############################################################################
def zero_space(event):
    u = state['u']
    v = state['v']

    x0, x1 = range_slider.val

    u[x<x0] = 0
    u[x>x1] = 0

    v[x<x0] = 0
    v[x>x1] = 0

    state['u'] = u
    state['v'] = v

button_ax = plt.axes([0.4, 0.3, 0.2, 0.04])  # [left, bottom, width, height]
transform_button = Button(button_ax, 'Wipe Outside')
transform_button.on_clicked(zero_space)


##############################################################################
# Textbox for numerical input for perturbation
##############################################################################

axbox = plt.axes([0.775, 0.25, 0.05, 0.05])
text_box = TextBox(axbox, 'Perturb:', initial="0")

##############################################################################
#Button setup: perturbs within the space
##############################################################################

def perturb(event):
    try:
        u = state ['u']
        
        a = float(text_box.text)

        x0,x1 = range_slider.val

        u[(x >= x0) & (x <= x1)] = a

    except ValueError:
        print("Please enter a number!")


button_ax = plt.axes([0.7, 0.3, 0.2, 0.04])  # [left, bottom, width, height]
perturb_button = Button(button_ax, 'Perturb Inside')
perturb_button.on_clicked(perturb)

##############################################################################
###Wiping Space button
#############################################################################
#slider allows us to indicate where the space is wiped
range_ax = plt.axes([0.1, 0.35, 0.7, 0.03])  # full-width at the bottom
range_slider = RangeSlider(
    ax=range_ax,
    label="",
    valmin=x.min(),
    valmax=x.max(),
    valinit=(0, 200),
)


# Get bounding box of the range_ax in figure coordinates
bbox = range_ax.get_position()

# Place custom label just above the slider
fig.text(
    x=(bbox.x0 + bbox.x1) / 2,  # center of the slider
    y=bbox.y1 + 0.01,           # a little above the top edge
    s="Space Selector",
    ha='center',
    va='bottom',
    fontsize=10
)


# Initial rectangle parameters
left,right = range_slider.val

rect = patches.Rectangle(
    (left, -100), #bottom left corner coords
    right-left, #Width
    200,     #height
    linewidth=1,
    edgecolor='red',
    facecolor='black',
    alpha=0.2
)

ax.add_patch(rect)

##############################################################################
# Button setup for saving
##############################################################################
"""
def save(event):
    u, v, t, c, I = (state['u'],
            state['v'],
            state['t'],
            c_slider.val,
            I_slider.val)


    model = radio.value_selected

    savename = str("SaveFiles/" + 
            str(radio.value_selected).replace(" ","_") + 
            ".npz")

    np.savez(savename,
            x=x,
            u=u,
            v=v,
            t=t,
            I=I,
            c=c,
            model=model)

#Button to save data
savebutton_ax = plt.axes([0.25,0.01,0.5,0.04])
savebutton = Button(savebutton_ax,'Save the sysem')
savebutton.on_clicked(save)
"""


##############################################################################
# Button for toggling play and pause
##############################################################################

def toggle_play(event):
    global PLAY
    PLAY = not PLAY
    if PLAY:
        play_button.label.set_text("Pause")
    else:
        play_button.label.set_text("Play")
    #plt.draw()


play_ax = plt.axes([0.25,0.01,0.5,0.04])  # Adjust position as needed
play_button = Button(play_ax, 'Pause' if PLAY else 'Play')
play_button.on_clicked(toggle_play)


##############################################################################
# Checkbox setup for finding the stationary speed
##############################################################################

clist = []
maxima_x = []
dc = 0.1
def Create_CList(event):
    global clist
    global maxima_x
    global dc

    clist = []
    maxima_x = []
    dc = 0.1

#checkbox: automatically adjusts c
checkbox_ax = plt.axes([0.8,0.7,0.2,0.1]) #left, bottom, width and height
checkbox = CheckButtons(checkbox_ax,['Find speed'],[False])
checkbox.on_clicked(Create_CList)

##############################################################################
# Radio setup for changing the employed model
##############################################################################

# Add RadioButtons
rax = plt.axes([0.8, 0.5, 0.2, 0.15])  # [left, bottom, width, height]
radio = RadioButtons(rax, tuple(model_map.keys()))

# Event handler
def select_model(label):
    global model_func
    model_func = model_map[label]
   
    if model_func == FHN:
        ax.set_ylim(-0.5,1)

    elif model_func in (Karma_Ramp,Karma_Step):
        ax.set_ylim([-1, 6])

    else:
        ax.set_ylim([-100,100])#[-1, 6])


    #Run this so if the algorithm is being run it ets reset
    Create_CList(True)


radio.on_clicked(select_model)













##############################################################################
# Main loop
##############################################################################

def is_near_integer(t,tol):
    return abs(t - round(t)) < tol

# Animation function
def update_plot(frame):
    #Get the state of the system
    u, v, t, c, I = (state['u'],
            state['v'],
            state['t'],
            c_slider.val,
            I_slider.val)

    if PLAY:
        u_new,v_new = model_func(u,v,c,I)

        #Only update the image when time condition met
        if is_near_integer(t,tol=1e-6):

            #Monitor the peak and adjust c accordingly
            if checkbox.get_status()[0] == True:
                global dc


                if model_func == Morris_Lecar:
                    #print(maxima_x)

                    new_max = np.argmax(u_new)

                else:
                    new_max = np.argmax(v_new)#v_new)

                #populate a list of locations of maxima
                if len(maxima_x) ==0:
                    maxima_x.append(new_max)

                elif maxima_x[-1] != new_max:
                    maxima_x.append(new_max)
                
                    
                    #If moving to the right
                    if maxima_x[-1] > maxima_x[-2]:
                        c_prev = copy.copy(c)
                        c_new = c + dc

                        if c_new in clist:
                            dc/=10
                        c += dc
                        c_slider.set_val(c)
                        clist.append(c)
                        print(c)

                    #If moving to the left
                    elif maxima_x[-1] < maxima_x[-2]:
                        c_prev = copy.copy(c)
                        c_new = c - dc

                        if c_new in clist:
                            dc/=10
                        c -= dc
                        c_slider.set_val(c)
                        clist.append(c)
                        print(c)

            line_u.set_ydata(u_new)
            line_v.set_ydata(v_new)
            title.set_text(f"t={t:.2f}, c={c:.5f}")

        """
        #Update the visualisation rectangle
        left,right =  range_slider.val
        rect.set_x(left)
        rect.set_width(right-left)
        fig.canvas.draw_idle()
        """

        #Change state
        state['u'],state['v'],state['t'],state['c'] = u_new, v_new, t + dt, c




    #Update the visualisation rectangle
    left,right =  range_slider.val
    rect.set_x(left)
    rect.set_width(right-left)
    fig.canvas.draw_idle()



    return line_u, line_v, title

ani = animation.FuncAnimation(fig, update_plot, interval=30)
#plt.show()











##############################################################################
#Add Tkinter functionality for loading andsaving file
##############################################################################

def load_file():
    filepath = filedialog.askopenfilename(
        title="Open File",
        filetypes=[("NumPy files", "*.npz"), ("All files", "*.*")]
    )
    if filepath:
        print(f"Selected file: {filepath}")
        # Example of loading data:
        try:
            data = np.load(filepath)
            print(f"File contains: {list(data.keys())}")

            state['u'] = data['u']
            state['v'] = data['v']
            state['t'] = float(data['t'])
            state['c'] = data['c']
            state['I'] = data['I']

            model = str(data['model'])

    
            for i in state.keys():
                print(state[i])

            #Set values
            I_slider.set_val(data['I'])
            c_slider.set_val(data['c'])

            for i, label in enumerate(radio.labels):
                if label.get_text() == model:
                    radio.set_active(i)
                    select_model(model)
                    break

            #Set the data
            line_u.set_ydata(state['u'])
            line_v.set_ydata(state['v'])

            #Pause to avoid issues
            if PLAY:
                toggle_play(True)

            # You can now use data['u'], data['v'], etc. as needed
        except Exception as e:
            print(f"Failed to load file: {e}")




def save_file():
    # Suggest default filename based on model
    default_filename = str(radio.value_selected).replace(" ", "_") + ".npz"

    filepath = filedialog.asksaveasfilename(
        title="Save File As",
        defaultextension=".npz",
        initialfile=default_filename,
        filetypes=[("NumPy files", "*.npz"), ("All files", "*.*")]
    )
    if filepath:
        try:
            u, v, t, c, I = (
                state['u'],
                state['v'],
                state['t'],
                c_slider.val,
                I_slider.val
            )
            model = radio.value_selected

            np.savez(filepath,
                     x=x,
                     u=u,
                     v=v,
                     t=t,
                     I=I,
                     c=c,
                     model=model)
            print(f"Saved successfully to {filepath}")
        except Exception as e:
            print(f"Failed to save file: {e}")



#############################################################################
# Menu bar
#############################################################################

menu_bar = tk.Menu(root)
file_menu = tk.Menu(menu_bar, tearoff=0)

file_menu.add_command(label="Load", command=load_file)
file_menu.add_command(label="Save", command=save_file)

menu_bar.add_cascade(label="File", menu=file_menu)

# Add the menu bar to the root window
root.config(menu=menu_bar)

root.mainloop()
