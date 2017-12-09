# -*- coding: utf-8 -*-
"""
Created on Mon Dec  4 14:44:38 2017

@author: Aeint Thet Ngon
"""

from collections import OrderedDict
from math import log, sqrt
from bokeh.models import HoverTool

import numpy as np
import pandas as pd

from bokeh.plotting import figure, show, output_file

df=pd.read_csv("india_terrorism.csv")

n_color = OrderedDict([
    ("total",   "#0d3362"),
    ("totalwound", "#c64737"),
    ("totalkilled",     "black"  ),
])

gram_color = {
    "high" : "#aeaeb8",
    "low" : "#e69584",
}

hover = HoverTool(tooltips=[
    ("City", "@text"),
],
mode='vline')


width = 800
height = 800
inner_radius = 90
outer_radius = 300 - 10

minr = sqrt(log(1))
maxr = sqrt(log(1600))
a = (outer_radius - inner_radius) / (minr - maxr)
b = inner_radius - a * maxr

def rad(mic):
    return a * np.sqrt(np.log(mic)) + b

big_angle = 2.0 * np.pi / (len(df) + 1)
small_angle = big_angle / 7

p = figure(plot_width=width, plot_height=height, title="Cities in India (with more than 10 attacks that has victims that are wounded or killed)",
    x_axis_type=None, y_axis_type=None,
    x_range=(-420, 420), y_range=(-420, 420),
    min_border=0, outline_line_color="black",
    background_fill_color="#f0e1d2")

p.xgrid.grid_line_color = None
p.ygrid.grid_line_color = None

# annular wedges
angles = np.pi/2 - big_angle/2 - df.index.to_series()*big_angle
colors = [gram_color[gram] for gram in df.highlow]
p.annular_wedge(
    0, 0, inner_radius, outer_radius, -big_angle+angles, angles, color=colors,
)
# small wedges
p.annular_wedge(0, 0, inner_radius, rad(df.total),
                -big_angle+angles+5*small_angle, -big_angle+angles+6*small_angle,
                color=n_color['total'])
p.annular_wedge(0, 0, inner_radius, rad(df.totalwound),
                -big_angle+angles+3*small_angle, -big_angle+angles+4*small_angle,
                color=n_color['totalwound'])
p.annular_wedge(0, 0, inner_radius, rad(df.totalkilled),
                -big_angle+angles+1*small_angle, -big_angle+angles+2*small_angle,
                color=n_color['totalkilled'])

labels = np.power(10.0, np.arange(0, 5))
radii = a * np.sqrt(np.log(labels)) + b
p.circle(0, 0, radius=radii, fill_color=None, line_color="white")

'''
p.text(0, radii[:-1], [str(r) for r in labels[:-1]],
       text_font_size="8pt", text_align="center", text_baseline="middle")
'''
# radial axes
p.annular_wedge(0, 0, inner_radius-10, outer_radius+10,
                -big_angle+angles, -big_angle+angles, color="black")

# bacteria labels
xr = radii[0]*np.cos(np.array(-big_angle/2 + angles))
yr = radii[0]*np.sin(np.array(-big_angle/2 + angles))
label_angle=np.array(-big_angle/2+angles)
label_angle[label_angle < -np.pi/2] += np.pi # easier to read labels on the left side
p.text(xr, yr, df.city, angle=label_angle,
       text_font_size="9pt", text_align="center", text_baseline="middle")

# OK, these hand drawn legends are pretty clunky, will be improved in future release
p.circle([-40, -40], [-370, -390], color=list(gram_color.values()), radius=5)
p.text([-30, -30], [-370, -390], text=["Success rate-" + gr for gr in gram_color.keys()],
       text_font_size="7pt", text_align="left", text_baseline="middle")

p.rect([-40, -40, -40], [18, 0, -18], width=30, height=13,
       color=list(n_color.values()))
p.text([-15, -15, -15], [18, 0, -18], text=list(n_color),
       text_font_size="9pt", text_align="left", text_baseline="middle")

output_file("burtin_india.html", title="Cities in India (with more than 10 attacks that has victims that are wounded or killed)")


show(p)