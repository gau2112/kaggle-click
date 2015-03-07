# -*- coding: utf-8 -*-
"""
Created on Thu Oct 30 10:37:04 2014

@author: whale

dataset class
"""
import functools

from pylearn2.datasets.dataset import Dataset
from pylearn2.utils import wraps
import logging
import warnings
import cPickle
# data are in pytables
try:
    import tables
except ImportError:
    warnings.warn("data are in pytables")
import theano
floatX = theano.config.floatX
logger = logging.getLogger(__name__)
from pylearn2.space import CompositeSpace, VectorSpace, IndexSpace
from pylearn2.utils import safe_zip
from pylearn2.utils.exc import reraise_as
from pylearn2.utils.iteration import (
    FiniteDatasetIterator,
    resolve_iterator_class
)


class CLICK4DAY(Dataset):
    """
    Only for faster access there is a copy of hdf5 file in current directory
    but it mean to be only readable.  If you wish to modify the data, you
    should pass a local copy to the path argument.

    Parameters
    ----------
    which_set : WRITEME
    which_day : WRITEME
    path : WRITEME
    """
    
    
    def __init__(self, which_set, which_day, 
                 path = None):

        self.daylist = range(21,32)
        self.mapper = {'train': 0, 'valid': 1, 'test': 2}
        assert which_set in self.mapper.keys()
        assert which_day in self.daylist
        
        f = open('/home/whale/Documents/click/dayrows.pkl', 'r')
        self.dayrows = cPickle.load(f)
        f.close()
    
        self.__dict__.update(locals())
        del self.self

        if path is not None:
            raise NotImplementedError("Data path is the current directory.")

        # load data
        file_n = "click_data.h5"
        self.h5file = tables.open_file(file_n, mode = 'r')
        
        if which_set == 'test':
            test_group = self.h5file.root.test.test_raw
            self.X = test_group.X_t
            self.y = None
            self.samples = slice(0, self.X.shape[0])
            self.sample_index = self.samples.start
            self.examples = self.X.shape[0]
        else:
            train_group = self.h5file.root.train.train_raw
            self.X = train_group.X
            self.y = train_group.y
            self.samples = slice(sum(self.dayrows[:which_day-21]), sum(self.dayrows[:which_day-20]))
            self.sample_index = self.samples.start            
            self.examples = self.dayrows[which_day-21]
            
        max_labels = 2
        
        X_source = 'features'
        X_space = VectorSpace(dim=23)
        if self.y is None:
            space = X_space
            source = X_source
        else:
            y_space = IndexSpace(dim=1, max_labels=max_labels)
            y_source = 'targets'
        space = CompositeSpace((X_space, y_space))
        source = (X_source, y_source)
        self.data_specs = (space, source)
        self.X_space = X_space 
               
        self._iter_mode = resolve_iterator_class('sequential')
        self._iter_topo = False
        self._iter_targets = False
        self._iter_data_specs = (self.X_space, 'features')
        
        
    @wraps(Dataset.iterator)
    def iterator(self, mode=None, batch_size=None, num_batches=None,
                 topo=None, targets=None, rng=None, data_specs=None,
                 return_tuple=False):

        if topo is not None or targets is not None:
            warnings.warn("Usage of `topo` and `target` arguments are "
                          "being deprecated, and will be removed "
                          "around November 7th, 2013. `data_specs` "
                          "should be used instead. Here these two "
                          "arguments are not used",
                          stacklevel=2)

        if data_specs is None:
            data_specs = self._iter_data_specs

        # If there is a view_converter, we have to use it to convert
        # the stored data for "features" into one that the iterator
        # can return.
        space, source = data_specs
        if isinstance(space, CompositeSpace):
            sub_spaces = space.components
            sub_sources = source
        else:
            sub_spaces = (space,)
            sub_sources = (source,)

        convert = []
        for sp, src in safe_zip(sub_spaces, sub_sources):
            if src == 'features' and \
               getattr(self, 'view_converter', None) is not None:
                conv_fn = (lambda batch, self=self, space=sp:
                           self.view_converter.get_formatted_batch(batch,
                                                                   space))
            else:
                conv_fn = None

            convert.append(conv_fn)

        # TODO: Refactor
        if mode is None:
            if hasattr(self, '_iter_subset_class'):
                mode = self._iter_subset_class
            else:
                raise ValueError('iteration mode not provided and no default '
                                 'mode set for %s' % str(self))
        else:
            mode = resolve_iterator_class(mode)

        if batch_size is None:
            batch_size = getattr(self, '_iter_batch_size', None)
        if num_batches is None:
            num_batches = getattr(self, '_iter_num_batches', None)
        if rng is None and mode.stochastic:
            rng = self.rng
        return FiniteDatasetIterator(self,
                                     mode(self.examples, batch_size, num_batches,\
                                          self.sample_index, rng),
                                     data_specs=data_specs,
                                     return_tuple=return_tuple,
                                     convert=convert)
    
    def get_data(self):
        """
        Returns all the data, as it is internally stored.
        The definition and format of these data are described in
        `self.get_data_specs()`.

        Returns
        -------
        data : numpy matrix or 2-tuple of matrices
            The data
        """
        if self.y is None:
            return self.X
        else:
            return (self.X, self.y)
            
    def get_design_matrix(self):
        """
        .. todo::

            WRITEME
        """
        return self.X

    @wraps(Dataset.get_batch_design)
    def get_batch_design(self, batch_size, include_labels=False):
        """Method inherited from Dataset"""
        try:
            idx = self.rng.randint(self.X.shape[0] - batch_size + 1)
        except ValueError:
            if batch_size > self.X.shape[0]:
                reraise_as(ValueError("Requested %d examples from a dataset "
                                      "containing only %d." %
                                      (batch_size, self.X.shape[0])))
            raise
        rx = self.X[idx:idx + batch_size, :]
        if include_labels:
            if self.y is None:
                return rx, None
            ry = self.y[idx:idx + batch_size]
            return rx, ry
        #rx = numpy.cast[theano.config.floatX](rx)
        return rx

    @functools.wraps(Dataset.get_num_examples)
    def get_num_examples(self):
        
        return self.examples

    def has_targets(self):
        """ Returns true if the dataset includes targets """

        return self.y is not None
        
    def get_data_specs(self):
        """
        Returns the data_specs specifying how the data is internally stored.

        This is the format the data returned by `self.get_data()` will be.
        """
        return self.data_specs


class CLICK9DAYS(Dataset):
    """
    Only for faster access there is a copy of hdf5 file in current directory
    but it mean to be only readable.  If you wish to modify the data, you
    should pass a local copy to the path argument.

    Parameters
    ----------
    which_set : WRITEME
    path : WRITEME
    """
    
    
    def __init__(self, which_set, path = None):

        self.mapper = {'train': 0, 'valid': 1, 'test': 2}
        assert which_set in self.mapper.keys()
        
        self.__dict__.update(locals())
        del self.self

        if path is not None:
            raise NotImplementedError("Data path is the current directory.")

        # load data
        file_n = "click_data.h5"
        self.h5file = tables.open_file(file_n, mode = 'r')
        
        if which_set == 'test':
            test_group = self.h5file.root.test.test_raw
            self.X = test_group.X_t
            self.y = None
            
        else:
            train_group = self.h5file.root.train.train_raw
            if which_set == 'train':
                self.X = train_group.X_train
                self.y = train_group.y_train
                
            else :
                self.X = train_group.X_valid
                self.y = train_group.y_valid
                
        self.samples = slice(0, self.X.shape[0])
        self.sample_index = self.samples.start
        self.examples = self.X.shape[0]
            
        max_labels = 2
        
        X_source = 'features'
        X_space = VectorSpace(dim=23)
        if self.y is None:
            space = X_space
            source = X_source
        else:
            y_space = IndexSpace(dim=1, max_labels=max_labels)
            y_source = 'targets'
        space = CompositeSpace((X_space, y_space))
        source = (X_source, y_source)
        self.data_specs = (space, source)
        self.X_space = X_space 
               
        self._iter_mode = resolve_iterator_class('sequential')
        self._iter_topo = False
        self._iter_targets = False
        self._iter_data_specs = (self.X_space, 'features')
        self._iter_subset_class = resolve_iterator_class('even_sequential')
        
    @wraps(Dataset.iterator)
    def iterator(self, mode=None, batch_size=None, num_batches=None,
                 topo=None, targets=None, rng=None, data_specs=None,
                 return_tuple=False):

        if topo is not None or targets is not None:
            warnings.warn("Usage of `topo` and `target` arguments are "
                          "being deprecated, and will be removed "
                          "around November 7th, 2013. `data_specs` "
                          "should be used instead. Here these two "
                          "arguments are not used",
                          stacklevel=2)

        if data_specs is None:
            data_specs = self._iter_data_specs

        # If there is a view_converter, we have to use it to convert
        # the stored data for "features" into one that the iterator
        # can return.
        space, source = data_specs
        if isinstance(space, CompositeSpace):
            sub_spaces = space.components
            sub_sources = source
        else:
            sub_spaces = (space,)
            sub_sources = (source,)

        convert = []
        for sp, src in safe_zip(sub_spaces, sub_sources):
            if src == 'features' and \
               getattr(self, 'view_converter', None) is not None:
                conv_fn = (lambda batch, self=self, space=sp:
                           self.view_converter.get_formatted_batch(batch,
                                                                   space))
            else:
                conv_fn = None

            convert.append(conv_fn)

        # TODO: Refactor
        if mode is None:
            if hasattr(self, '_iter_subset_class'):
                mode = self._iter_subset_class
            else:
                raise ValueError('iteration mode not provided and no default '
                                 'mode set for %s' % str(self))
        else:
            mode = resolve_iterator_class(mode)

        if batch_size is None:
            batch_size = getattr(self, '_iter_batch_size', None)
        if num_batches is None:
            num_batches = getattr(self, '_iter_num_batches', None)
        #if rng is None:
         #   rng = self.rng
        return FiniteDatasetIterator(self,
                                     mode(self.examples, batch_size, num_batches, self.sample_index, rng),
                                     data_specs=data_specs,
                                     return_tuple=return_tuple,
                                     convert=convert)
    
    def get_data(self):
        """
        Returns all the data, as it is internally stored.
        The definition and format of these data are described in
        `self.get_data_specs()`.

        Returns
        -------
        data : numpy matrix or 2-tuple of matrices
            The data
        """
        if self.y is None:
            return self.X
        else:
            return (self.X, self.y)
            
    def get_design_matrix(self):
        """
        .. todo::

            WRITEME
        """
        return self.X

    @wraps(Dataset.get_batch_design)
    def get_batch_design(self, batch_size, include_labels=False):
        """Method inherited from Dataset"""
        try:
            idx = self.rng.randint(self.X.shape[0] - batch_size + 1)
        except ValueError:
            if batch_size > self.X.shape[0]:
                reraise_as(ValueError("Requested %d examples from a dataset "
                                      "containing only %d." %
                                      (batch_size, self.X.shape[0])))
            raise
        rx = self.X[idx:idx + batch_size, :]
        if include_labels:
            if self.y is None:
                return rx, None
            ry = self.y[idx:idx + batch_size]
            return rx, ry
        #rx = numpy.cast[theano.config.floatX](rx)
        return rx

    @functools.wraps(Dataset.get_num_examples)
    def get_num_examples(self):
        
        return self.examples

    def has_targets(self):
        """ Returns true if the dataset includes targets """

        return self.y is not None
        
    def get_data_specs(self):
        """
        Returns the data_specs specifying how the data is internally stored.

        This is the format the data returned by `self.get_data()` will be.
        """
        return self.data_specs
