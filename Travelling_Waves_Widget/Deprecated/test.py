import tkinter as tk
from tkinter import ttk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.widgets import Slider, Button, TextBox
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np

# Dummy model function for illustration
def model_func(u, v, c, I):
    return u + 0.01, v + 0.01

# Example is_near_integer
def is_near_integer(t, tol=1e-6):
    return abs(t - round(t)) < tol

# Initial states
state = {
    'u': np.zeros(100),
    'v': np.ones(100),
    't': 0,
    'c': 0.0,
    'I': 0.0
}
dt = 0.1
maxima_x, clist = [], []
dc = 0.1

# Create tkinter window
root = tk.Tk()
root.title("Interactive PDE Simulation")

# Create matplotlib figure
fig, ax = plt.subplots()
plt.subplots_adjust(bottom=0.3)
x = np.linspace(0, 1, 100)
line_u, = ax.plot(x, state['u'], label='u')
line_v, = ax.plot(x, state['v'], label='v')
title = ax.set_title("t=0.00, c=0.00")
ax.legend()

# Embed figure in tkinter
canvas = FigureCanvasTkAgg(fig, master=root)
canvas_widget = canvas.get_tk_widget()
canvas_widget.grid(row=0, column=0, columnspan=4)

# Slider for c
ax_slider = plt.axes([0.2, 0.15, 0.65, 0.03])
c_slider = Slider(ax_slider, 'Speed c', -3.0, 10.0, valinit=0.0)

def on_c_slider(val):
    state['c'] = val
c_slider.on_changed(on_c_slider)

# Slider for I
ax_Islider = plt.axes([0.2, 0.1, 0.65, 0.03])
I_slider = Slider(ax_Islider, 'Input I', -10.0, 10.0, valinit=0.0)

def on_I_slider(val):
    state['I'] = val
I_slider.on_changed(on_I_slider)

# Text box
axbox = plt.axes([0.9, 0.25, 0.08, 0.05])
text_box = TextBox(axbox, 'Perturb:', initial="0")

# Dummy checkbox
checkbox_val = tk.BooleanVar()
checkbox = ttk.Checkbutton(root, text="Auto Adjust c", variable=checkbox_val)
checkbox.grid(row=1, column=0)

# Update function
def update_plot(frame):
    u, v, t, c, I = state['u'], state['v'], state['t'], state['c'], state['I']
    u_new, v_new = model_func(u, v, c, I)

    if is_near_integer(t):
        if checkbox_val.get():
            global dc
            new_max = np.argmax(v_new)
            if maxima_x == [] or maxima_x[-1] != new_max:
                maxima_x.append(new_max)
                if len(maxima_x) >= 2:
                    direction = np.sign(maxima_x[-1] - maxima_x[-2])
                    c_new = c + direction * dc
                    if c_new in clist:
                        dc /= 10
                    c += direction * dc
                    c_slider.set_val(c)
                    clist.append(c)

    line_u.set_ydata(u_new)
    line_v.set_ydata(v_new)
    title.set_text(f"t={t:.2f}, c={c:.2f}")
    state['u'], state['v'], state['t'] = u_new, v_new, t + dt
    return line_u, line_v, title

# Animation
ani = animation.FuncAnimation(fig, update_plot, interval=100, blit=False)

# Start tkinter loop
root.mainloop()

