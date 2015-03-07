# -*- coding: utf-8 -*-
"""
Created on Thu Oct 30 17:14:20 2014

@author: whale
"""

from pylearn2.datasets.transformer_dataset import TransformerDataset
#from numpy.random import RandomState

class CLICKTransformerDataset(TransformerDataset):
    """
    A TransformerDataset that takes images from GALAXY and rotates, crops, 
    downsamples and splits each of them to 16* small images.
    
    Parameters
    ----------
    raw : pylearn2 Dataset
        raw dataset.
    transformer: transformer class
        transformer to transform data
    seed : integer or list of integers, optional
        The seed passed to RandomStreams to make the Bernoulli
        samples. If not specified, all class instances default to
        the same seed so two instances can be run synchronized side
        by side.
    """

    def __init__(self, raw, transformer, seed=1988):
    
        self.raw = raw
        #self.rng = RandomState(seed=seed)
        
        self.transformer = transformer

        super(CLICKTransformerDataset, self).__init__(self.raw, self.transformer,
                                                       space_preserving=False)

        self.__dict__.update(locals())
        del self.self

    def get_design_matrix(self, topo=None):
        """
        .. todo::

            WRITEME
        """
        if topo is not None:
            raise NotImplementedError("hope not use this method")
        X = self.raw.get_design_matrix()
        return self.transformer.perform(X)

    
