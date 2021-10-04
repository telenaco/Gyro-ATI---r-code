import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time


plt.rcParams["figure.dpi"] = (72*4)

df = pd.read_csv("2021-09-22_19-57-16_gyrodata_clean.csv")


diskMass = 0.050        # weight in grams for one disk
diskRadius = 0.05       # 10 cm diamter
diskAngVel = 753        # max vel at constant speed on rad/s (7200rpm)
I = diskMass*diskRadius*diskRadius

Imatrix = np.array([[(0.25*diskMass*diskRadius*diskRadius), 0, 0],
                    [0, (0.25*diskMass*diskRadius*diskRadius), 0],
                    [0, 0, (0.5*diskMass*diskRadius*diskRadius)]])


# to clean the data before running the calcualtion of the equaition not really need it 
# df.loc[0:14100, "accYaw"] = 0
# df.loc[0:14100, "velYaw"] = 0
# df.loc[0:14100, "radYaw"] = 0
# df.loc[0:14100, "degYaw"] = 0

# df.loc[13850:27150, "accYaw"] = 0
# df.loc[13850:27150, "velYaw"] = 0
# df.loc[13850:27150, "radYaw"] = 1.5708
# df.loc[13850:27150, "degYaw"] = 90

# df.loc[12480:14950, "accPitch"] = 0
# df.loc[12480:14950, "velPitch"] = 0
# df.loc[12480:14950, "radPitch"] = 0
# df.loc[12480:14950, "degPitch"] = 0

# df.loc[25975:, "accPitch"] = 0
# df.loc[25975:, "velPitch"] = 0
# df.loc[25975:, "radPitch"] = 1.5708
# df.loc[25975:, "degPitch"] = 90

df["lgEqX"] = 0
df["lgEqY"] = 0
df["lgEqZ"] = 0
df["shEqX"] = 0
df["shEqY"] = 0
df["shEqZ"] = 0

# get starting time
start = time.time()
for i, row in enumerate(df.values):

    # long equation for the output torque
    ##########################################################################

    omegaDotDisk = np.array([ [(-(df.loc[i, "accYaw"])*(np.sin(df.loc[i, "radPitch"])))-((df.loc[i, "velYaw"])*(df.loc[i, "velPitch"])*(np.cos(df.loc[i, "radPitch"])))],
                              [(df.loc[i, "accPitch"])],
                              [((df.loc[i, "accYaw"])*(np.cos(df.loc[i, "radPitch"])))-((df.loc[i, "velYaw"])*(df.loc[i, "velPitch"])*(np.sin(df.loc[i, "radPitch"])))] ])

    ##########################################################################

    omegaDisk = np.array([[-(df.loc[i, "velYaw"])*(np.sin(df.loc[i, "radPitch"]))],
                          [(df.loc[i, "velPitch"])],
                          [diskAngVel + ((df.loc[i, "velYaw"])*(np.cos(df.loc[i, "radPitch"])))]])

    ##########################################################################

    omegaGimbal = np.array([[-(df.loc[i, "velYaw"])*(np.sin(df.loc[i, "radPitch"]))],
                            [(df.loc[i, "velPitch"])],
                            [(df.loc[i, "velYaw"]) * (np.cos(df.loc[i, "radPitch"]))]])

    ##########################################################################

    tmp1 = Imatrix.dot(omegaDotDisk)

    tmp2 = Imatrix.dot(omegaDisk)

    tmp3 = np.cross(omegaGimbal, tmp2, axis=0)

    longEqTorque = tmp1 + tmp3
    
    # short equation for output torque

    shEqX = 1/4*I * ((2*(df.loc[i, "velPitch"]) * diskAngVel) - ((df.loc[i, "accYaw"]) * (np.sin(df.loc[i, "radPitch"]))))
    shEqY = 1/4*I * ( (df.loc[i, "accPitch"]) +((df.loc[i, "velYaw"])*(np.sin(df.loc[i, "radPitch"]))*((2*diskAngVel)+((df.loc[i, "velYaw"])*(np.cos(df.loc[i, "radPitch"]))))))
    shEqZ = 1/2*I * (((df.loc[i, "accYaw"])*(np.cos(df.loc[i, "radPitch"])))-((df.loc[i, "velPitch"])*(df.loc[i, "velYaw"])*(np.sin(df.loc[i, "radPitch"]))))
    
    shEq = np.array([[shEqX],
                     [shEqY],
                     [shEqZ]])

    # rotation to hand 

    rot_a_to_d = np.array([[(np.cos(df.loc[i, "radPitch"]))*(np.cos(df.loc[i, "radYaw"])),     -(np.sin(df.loc[i, "radYaw"])),              (np.sin(df.loc[i, "radPitch"]))*(np.cos(df.loc[i, "radYaw"]))],
                           [(np.cos(df.loc[i, "radPitch"]))*(np.sin(df.loc[i, "radYaw"])),      (np.cos(df.loc[i, "radYaw"])),              (np.sin(df.loc[i, "radPitch"]))*(np.sin(df.loc[i, "radYaw"]))],
                           [(np.sin(df.loc[i, "radPitch"])),                                     0,                                         (np.cos(df.loc[i, "radPitch"]))]])

    longEqTorque = rot_a_to_d.dot(longEqTorque)
    shEq = rot_a_to_d.dot(shEq)

    df.loc[i, "lgEqX"] = longEqTorque.item(0)
    df.loc[i, "lgEqY"] = longEqTorque.item(1)
    df.loc[i, "lgEqZ"] = longEqTorque.item(2)
    
    df.loc[i, "shEqX"] = shEq.item(0)
    df.loc[i, "shEqY"] = shEq.item(1)
    df.loc[i, "shEqZ"] = shEq.item(2)
    
# get time taken to run the for loop code 
elapsed_time_fl = (time.time() - start) 
print (elapsed_time_fl)

fig, axes = plt.subplots(nrows=3)
df.plot(ax=axes[0],y="lgEqX", x="millisElapsed",figsize=(10,6),  color={"lgEqX": "orange"}, lw=0.5)
df.plot(ax=axes[1],y="shEqX", x="millisElapsed", color={"shEqX": "blue"}, lw=0.5)
df.plot(ax=axes[2],y="x1", x="millisElapsed",color={"x1": "red"}, lw=0.5)


df.to_csv("pythonUpdateGyroActuation.csv")


############################################################################################

dfModel = pd.read_csv("pythonUpdateGyroActuation.csv")
dfAti = pd.read_csv("2021-09-22_19-57-16_atiData_clean.csv")

fig, axes = plt.subplots(nrows=4)
dfModel.plot(ax=axes[0],y="lgEqX", x="millisElapsed",figsize=(14, 8),  color={"lgEqX": "orange"}, lw=0.5).legend(loc='upper left')
dfModel.plot(ax=axes[1],y="shEqX", x="millisElapsed", color={"shEqX": "blue"}, lw=0.5).legend(loc='upper left')
dfModel.plot(ax=axes[2],y="x1", x="millisElapsed",color={"x1": "red"}, lw=0.5).legend(loc='upper left')
dfAti.plot(ax=axes[3],y="Tx_f", x="millisElapsed", color={"Tx_f": "orange"}, lw=0.5).legend(loc='upper left')

fig, axes = plt.subplots(nrows=4)
dfModel.plot(ax=axes[0],y=["lgEqY"],figsize=(14, 8), x="millisElapsed", color={"lgEqY": "orange"}, lw=0.5).legend(loc='upper left')
dfModel.plot(ax=axes[1],y=["shEqY"], x="millisElapsed", color={"shEqY": "blue"}, lw=0.5).legend(loc='upper left')
dfModel.plot(ax=axes[2],y=["y1"], x="millisElapsed", color={"y1": "red"}, lw=0.5).legend(loc='upper left')
dfAti.plot(ax=axes[3],y=["Ty_f"], x="millisElapsed", color={"Ty_f": "orange"}, lw=0.5).legend(loc='upper left')

fig, axes = plt.subplots(nrows=4)
dfModel.plot(ax=axes[0],y=["lgEqZ"],figsize=(14, 8), x="millisElapsed", color={"lgEqZ": "orange"}, lw=0.5)
dfModel.plot(ax=axes[1],y=["shEqZ"], x="millisElapsed", color={"shEqZ": "blue"}, lw=0.5)
dfModel.plot(ax=axes[2],y=["z1"], x="millisElapsed", color={"z1": "red"}, lw=0.5)
dfAti.plot(ax=axes[3],y=["Tz_f"], x="millisElapsed", color={"Tz_f": "orange"}, lw=0.5)
                     
          
