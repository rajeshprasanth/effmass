#!/usr/bin/env python
#
#
import math
import numpy as np

#stencil = 5
#stencil = 9
stencil = 3


#overide pi
pi = 3.14159265358979323846



def frac2cart(a, b, c, alpha, beta, gamma, invect):
	#
	malpha = math.radians(alpha)
	mbeta = math.radians(beta)
	mgamma = math.radians(gamma)
	#
	v = a*b*c*math.sqrt(1-(math.cos(malpha)**2)-(math.cos(mbeta)**2)-(math.cos(mgamma))**2)+(2*math.cos(malpha)*math.cos(mbeta)*math.cos(mgamma))
	mat = np.array([[a, b*math.cos(mgamma), c*math.cos(mbeta)],
		[0, b*math.sin(mgamma), (c*(math.cos(malpha)-(math.cos(mbeta)*math.cos(mgamma))))/math.sin(mgamma)],
		[0, 0, v/(a*b*math.sin(mgamma))]])
	 
	return np.dot(mat,invect)
	
def cart2frac(a, b, c, alpha, beta, gamma, invect):
	print "Empty line"
	
def rad2deg(rad):
	return rad*(180/pi)
	
def deg2rad(deg):
	return deg*(pi/180)



	
kpoint=[0.5,0.5,0.0]
t=frac2cart(.5,.5,.5,600.0,60.0,60.0,kpoint)


print t
