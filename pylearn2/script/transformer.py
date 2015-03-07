# -*- coding: utf-8 -*-
"""
Created on Thu Oct 30 12:45:30 2014

@author: whale
"""

import numpy as np
#import gc
import cPickle
from pylearn2.blocks import Block
#from pylearn2.datasets.nominal2onehot import nominal2onehot
from pylearn2.space import CompositeSpace, VectorSpace
from sklearn import feature_extraction
import theano

class Transformer(Block):
    """
    .. todo::

        WRITEME

    Parameters
    ----------
    
    raw : WRITEME
        raw dataset
    nfeatures : WRITEME
        after transformed, number of features
    """
    
    
    
    def __init__(self, raw, nfeatures=1048576,  rng=None):
        
        self.raw = raw
        self.nfeatues = nfeatures
        
        '''
        # all feature names
        f = open('colnames.pkl', 'r')
        self.features = cPickle.load(f)
        f.close()
    
        # categories in each feature
        # no days
        f = open('colscate.pkl', 'rb')
        self.colcate = []
        for i in range(len(self.features)-1):
            self.colcate.append(cPickle.load(f))
        f.close()
        
        #assert rng is not None, ("rng must be a numpy RandomState instance")
        
        # index of features to use and transform
        self.featureindex = []
        for ele in featurelist:
            assert ele in self.features
            self.featureindex.append(np.where(self.features==ele))
        '''
        self.input_space = self.raw.data_specs[0]
        
        super(Transformer, self).__init__()
        
        self.__dict__.update(locals())
        del self.self
        
    def perform(self, inputs):
        """
        .. todo::

            WRITEME
        """
        rval = self._hashingtrick().transform([self._rowstring(row) for row in inputs])
        
        return rval
        
    def __call__(self, inputs):
        """
        .. todo::

            WRITEME
        """
        
        return self.perform(inputs)
                        
        
    def _theanofunction(self, inputs):
        
        components, updates = theano.scan(fn=lambda row: self._hashingtrick().fit_transform(self._rowstring(row)),
                                  outputs_info=None,
                                  sequences=[inputs])
        rval = theano.function(inputs=[inputs], outputs=components)
        
        return rval
        
    def _hashingtrick(self):
        fh = feature_extraction.FeatureHasher(n_features=self.nfeatures, \
                    input_type='string', dtype=np.float32, non_negative=False)
        return fh
        
    def _rowstring(self, row):
        """
            return a list of string, with every string formated as
            'namespace:featurevalue',
            except the 'day' column.
        """
        return ['{}:{}'.format(name,row[name]) for name in self.raw.X.colnames]
    
    def set_input_space(self, space):
        """
        .. todo::

            WRITEME
        """
        self.input_space = space

    def get_input_space(self):
        """
        .. todo::

            WRITEME
        """
        if self.input_space is not None:
            return self.input_space
        

    def get_output_space(self):
        """
        .. todo::

            WRITEME
        """
        if self.input_space is None:
            raise ValueError("No input space was specified for this Block (%s). "
                "You can call set_input_space to correct that." % str(self))
                
        if isinstance(self.input_space, CompositeSpace):
            X_space = VectorSpace(dim=self.nfeatures, sparse=True)
            y_space = self.input_space.components[1]
            output_space = CompositeSpace((X_space, y_space))
        elif isinstance(self.input_space, VectorSpace):
            output_space = VectorSpace(dim=self.nfeatures, sparse=True)
        else:
            raise TypeError("Can't transform IndexSpaces")
        
        return output_space
        
class TransformerClick(Block):
    """
    .. todo::

        WRITEME

    Parameters
    ----------
    
    raw : WRITEME
        raw dataset
    interaction : WRITEME
        whether to do interaction or not
    nfeatures : WRITEME
        number of features to be transformed
    """
    
    
    
    def __init__(self, raw, interaction=False, nfeatures=23, rng=None):
        
        self.raw = raw
        self.interaction = interaction
        
        self.nfeatures = nfeatures
        
        # global likelihood
        self.glh = 0.084933389586625302
        
        # click rate in each feature
        # no days
        f = open('clickratedic.pkl', 'rb')
        self.clickratedic = cPickle.load(f)
        f.close()        
        
        self.colnames = self.raw.X.colnames
        
        self.featureindex = {}
        i = 0
        for name in self.colnames:
            self.featureindex[name] = i
            i+=1
        
        self.input_space = self.raw.data_specs[0]
        
        super(TransformerClick, self).__init__()
        
        self.__dict__.update(locals())
        del self.self
        
    def perform(self, inputs):
        """
        .. todo::

            WRITEME
        """
        if self.interaction is False:
            rval = np.zeros((inputs.shape[0],self.nfeatures), dtype=np.float32)
            i = 0
            for row in inputs:
                for name in self.colnames:
                    if row[name] in self.clickratedic[name]:
                        rval[i][self.featureindex[name]] = self.clickratedic[name][row[name]]
                    else:
                        rval[i][self.featureindex[name]] = self.glh
                i+=1
        else:
            raise NotImplementedError()
                    
        return rval
        
    def __call__(self, inputs):
        """
        .. todo::

            WRITEME
        """
        
        return self.perform(inputs)
    
    def set_input_space(self, space):
        """
        .. todo::

            WRITEME
        """
        self.input_space = space

    def get_input_space(self):
        """
        .. todo::

            WRITEME
        """
        if self.input_space is not None:
            return self.input_space
        

    def get_output_space(self):
        """
        .. todo::

            WRITEME
        """
        if self.input_space is None:
            raise ValueError("No input space was specified for this Block (%s). "
                "You can call set_input_space to correct that." % str(self))
                
        if isinstance(self.input_space, CompositeSpace):
            X_space = VectorSpace(dim=self.nfeatures, sparse=False)
            y_space = self.input_space.components[1]
            output_space = CompositeSpace((X_space, y_space))
        elif isinstance(self.input_space, VectorSpace):
            output_space = VectorSpace(dim=self.nfeatures, sparse=False)
        else:
            raise TypeError("Can't transform IndexSpaces")
        
        return output_space
