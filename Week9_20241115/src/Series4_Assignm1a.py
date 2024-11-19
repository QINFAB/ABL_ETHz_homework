import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd

# -------------------------------------------------------------------------------
# -- Preliminaries. E.g., define constants

g = 9.81  # acceleration due to gravity (m s-2)
vK = 0.4  # von Karman constant


# -------------------------------------------------------------------------------
# -- Read data and define relevant variables

FilePath = "data/"   # specify here the path to where the data is stored
FileName = os.path.join(FilePath, "SurfaceData_Cabauw_05-10-May-2008.txt")
SFCData = pd.read_csv(FileName, delim_whitespace=True)


# #-- Inspect data structure
# SFCData.head()

z2 = 2
z5 = 5
z10 = 10

hour = SFCData.hour

T2 = SFCData.Ta002 + 273.15
U10 = SFCData.U010

ustar = SFCData.ust005
wT = SFCData.wT005


# -------------------------------------------------------------------------------
# -- Evaluate requested quantities

L = -ustar ** 3 / (vK * 9.81 / T2 * wT)

zoL = z5 / L


# -------------------------------------------------------------------------------
# -- Plot

fig, ax = plt.subplots(1, 1, figsize=(5, 3), layout="constrained")

ax.scatter(hour, L, s=3, color="gray")
ax.axhline(0, color="k")
ax.axvline(4, linestyle="-", color="b")
ax.axvline(19, linestyle="-", color="b")
ax.text(3, -120, s = "sunrise", color = "b", rotation = 90)
ax.text(19.5,-120, s = "sunset", color = "b", rotation = 90)
ax.axvline(6, linestyle="--", color="b")
ax.axvline(16.5, linestyle="--", color="b")
ax.set(title="Assignment 1a", xlim=(0, 24), ylim=(-150, 200), xlabel="time [h] (UTC)", ylabel="L [m]")


# ----

fig.savefig("./Assignm1_py.pdf")


