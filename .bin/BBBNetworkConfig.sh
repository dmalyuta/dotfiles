#!/bin/bash
# Setup network sharing with my BeagleBone Black

ifconfig

read -p "Main adapter name in above output: " mainAdapter
read -p "BeagleBone Black adapter name in above output: " BBBAdapter

sudo iptables --table nat --append POSTROUTING --out-interface $mainAdapter -j MASQUERADE
sudo iptables --append FORWARD --in-interface $BBBAdapter -j ACCEPT
sudo sh -c "echo 1 > /proc/sys/net/ipv4/ip_forward"
