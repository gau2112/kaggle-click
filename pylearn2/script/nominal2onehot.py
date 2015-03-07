# -*- coding: utf-8 -*-
"""
Created on Thu Oct 30 09:50:41 2014

@author: whale

Nominal data one hot encoded to binary sparse data
"""
import numpy as np

def nominal2onehot(x, xlist):
    rvl = np.zeros((len(xlist),), dtype=np.float32)
    if x in xlist:
        rvl[np.where(xlist==x)]=1
    return rvl
