import re
import matplotlib.pyplot as plt

FAN_CURVES="""
CPU: enabled: true, 47c:0%,49c:5%,51c:11%,57c:20%,60c:25%,63c:33%,66c:40%,70c:47%
GPU: enabled: true, 47c:0%,49c:5%,51c:11%,57c:18%,60c:24%,63c:31%,66c:37%,70c:44%
"""

REGEX=re.compile("([A-Z]+): .*: .*, ([0-9%c:,]+)")

fan_curves = dict()
for line in FAN_CURVES.splitlines():
    if line == "":
        continue
    matches = REGEX.search(line)
    if matches:
        component = matches.group(1)
        curve_str = matches.group(2)
        curve_nodes_str = [node.split(":") for node in curve_str.split(",")]
        temps = []
        pwms = []
        for node_str in curve_nodes_str:
            assert node_str[0][-1] == "c"
            assert node_str[1][-1] == "%"
            temps.append(float(node_str[0][:-1]))
            pwms.append(float(node_str[1][:-1]))
        fan_curves[component] = dict(x=temps, y=pwms)

fig = plt.figure()
plt.clf()
ax = fig.add_subplot(111)
for component, curve in fan_curves.items():
    ax.plot(curve["x"], curve["y"], label=component)
ax.legend(loc="upper left")
ax.autoscale(tight=True)
ax.grid()
ax.set_xlabel("Temperature [C]")
ax.set_ylabel("Fan PWM [%]")
ax.set_title("asusctl fan-curves")
