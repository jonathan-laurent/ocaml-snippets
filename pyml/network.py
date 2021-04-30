import torch
from torch import nn
import torch.nn.functional as F
import numpy as np

class MLP(nn.Module):
    """
    Multi-Layer Perceptron with constant width.
    """
    def __init__(self, in_features, out_features, width, depth):
        super(MLP, self).__init__()
        assert(depth >= 1)
        self.layers = nn.ModuleList([])
        for i in range(depth):
            layer = nn.Linear(
                in_features=(in_features if i == 0 else width),
                out_features=(out_features if i == depth-1 else width),
                bias=True)
            self.layers += [layer]

    def forward(self, input):
        t = input
        for layer in self.layers[:-1]:
            t = F.relu(layer(t))
        return self.layers[-1](t)

def make_mlp(in_features, out_features, width, depth):
  return MLP(in_features, out_features, width, depth)

def to_torch_tensor(x):
  return torch.tensor(x)

def of_torch_tensor(x):
  return x.detach().numpy()