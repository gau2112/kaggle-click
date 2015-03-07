"""
.. todo::

    WRITEME
"""
import functools

from pylearn2.datasets.dataset import Dataset
from pylearn2.utils import wraps
import logging
import numpy
import warnings
import cPickle
# data are in pytables
try:
    import tables
except ImportError:
    warnings.warn("data are in pytables")
try:
    import scipy.sparse
except ImportError:
    warnings.warn("Couldn't import scipy.sparse")
import theano
import gzip
floatX = theano.config.floatX
logger = logging.getLogger(__name__)
from pylearn2.space import CompositeSpace, VectorSpace, IndexSpace
from pylearn2.utils import safe_zip
from pylearn2.utils.exc import reraise_as
from pylearn2.utils.iteration import (
    FiniteDatasetIterator,
    resolve_iterator_class
)


class CLICKSparseDataset(Dataset):
    """
    SparseDataset is a class for representing datasets that can be
    stored as a sparse matrix.

    Parameters
    ----------
    X_load_path : str or None, optional
        the path to read the sparse dataset
        from_scipy_sparse_dataset is not used if load_path is specified
    X_from_scipy_sparse_dataset : matrix of type scipy.sparse or None, optional
        In case load_path is not provided,
        the sparse dataset is passed directly to the class by
        using from_scipy_sparse_dataset parameter.
    X_zipped_npy : bool, optional
        used only when load_path is specified.
        indicates whether the input matrix is zipped or not.
        defaults to True.
    y_path : str or None, optional
        the path of y.
    y_part : str or None, optional
        which day to be used.
    
    """

    _default_seed = (17, 2, 946)
    
    def __init__(self, X_load_path=None,
                 X_from_scipy_sparse_dataset=None, X_zipped_npy=False,
                 y_path=None, y_labels=None, y_part=None, rng=_default_seed):

        
        if X_load_path is not None:
            if X_zipped_npy is True:
                logger.info('... loading sparse data set from a zip npy file')
                self.X = scipy.sparse.csr_matrix(
                    numpy.load(gzip.open(X_load_path)), dtype=floatX)
            else:
                logger.info('... loading sparse data set from a npy file')
                loader = numpy.load(X_load_path)
                self.X = scipy.sparse.csr_matrix((loader['data'], \
                         loader['indices'], loader['indptr']), \
                         shape = loader['shape'], dtype=floatX)
        else:
            logger.info('... building from given sparse dataset')
            self.X = X_from_scipy_sparse_dataset
            if not scipy.sparse.issparse(X_from_scipy_sparse_dataset):
                msg = "from_scipy_sparse_dataset is not sparse : %s" \
                      % type(self.X)
                raise TypeError(msg)

        if y_path is not None:
            logger.info('... loading y data set from a hdf5 file')
            file_handler = tables.open_file(y_path, mode = "r")
            y = file_handler.root.train.train_raw.y
            assert y_part is not None
            f = open('dayrows.pkl', 'r')
            dayrows = cPickle.load(f)
            f.close()
            self.y = y[sum(dayrows[:y_part-1]):sum(dayrows[:y_part])]
        self.y_labels = y_labels
        
        X_source = 'features'
        X_space = VectorSpace(dim=self.X.shape[1], sparse=True)
    
        if y_path is None:
            space = X_space
            source = X_source
        else:
            if self.y.ndim == 1:
                dim = 1
            else:
                dim = self.y.shape[-1]
            if self.y_labels is not None:
                y_space = IndexSpace(dim=dim, max_labels=self.y_labels)
            else:
                y_space = VectorSpace(dim=dim)
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
                                     mode(self.X.shape[0],
                                          batch_size,
                                          num_batches,
                                          rng),
                                     data_specs=data_specs,
                                     return_tuple=return_tuple,
                                     convert=convert)

    '''
    def __iter__(self):
        """
        .. todo::

            WRITEME
        """
        return self

    def next(self):
        """
        .. todo::

            WRITEME
        """
        indx = self.subset_iterator.next()
        try:
            mini_batch = self.X[indx]
        except IndexError, e:
            reraise_as(ValueError("Index out of range"+str(e)))
            # the ind of minibatch goes beyond the boundary
        return mini_batch
        
    '''
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
        

    @wraps(Dataset.get_batch_topo)
    def get_batch_topo(self, batch_size):
        """Method inherited from Dataset"""
        raise NotImplementedError('Not implemented for sparse dataset')

    @functools.wraps(Dataset.get_num_examples)
    def get_num_examples(self):
        return self.X.shape[0]

    

    def has_targets(self):
        """ Returns true if the dataset includes targets """

        return self.y is not None
        
    def get_data_specs(self):
        """
        Returns the data_specs specifying how the data is internally stored.

        This is the format the data returned by `self.get_data()` will be.
        """
        return self.data_specs

    def get_data(self):
        """
        Returns
        -------
        data : numpy matrix or 2-tuple of matrices
            Returns all the data, as it is internally stored.
            The definition and format of these data are described in
            `self.get_data_specs()`.
        """
        if self.y is None:
            return self.X
        else:
            return (self.X, self.y)
