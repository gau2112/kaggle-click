# -*- coding: utf-8 -*-
"""
Created on Fri Nov 21 15:12:30 2014

@author: whale

for debugging perpose
for learning pylearn2
"""

from dataclassraw import CLICK4DAY
from transformer import Transformer
from pylearn2.datasets.transformer_dataset import TransformerDataset

from pylearn2.models import mlp
from pylearn2.training_algorithms import sgd
from pylearn2.termination_criteria import EpochCounter


raw_ds = CLICK4DAY(which_set='train', which_day=21)
transformer = Transformer(raw=raw_ds, nfeatures=1024,  rng=None)
ds = TransformerDataset(raw=raw_ds, transformer=transformer, cpu_only=False, \
                 space_preserving=False)


hidden_layer = mlp.Sigmoid(layer_name='hidden', dim=256, irange=.1, init_bias=1.)

output_layer = mlp.Softmax(2, 'output', irange=.1)

trainer = sgd.SGD(learning_rate=.05, batch_size=1024, \
train_iteration_mode='even_sequential',termination_criterion=EpochCounter(400))

layers = [hidden_layer, output_layer]

ann = mlp.MLP(layers, nvis=1024)

trainer.setup(ann, ds)

# train neural net until the termination criterion is true
while True:
    trainer.train(dataset=ds)
    ann.monitor.report_epoch()
    ann.monitor()
    if not trainer.continue_learning(ann):
        break
 
