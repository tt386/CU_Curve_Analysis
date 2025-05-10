# Code for analysis of C-U curves in excitable, spatially-extended models of cardiomyocyte activity 

## Executing code: 

Navigate to directory of the model you wosh to run, enter the auto environment, then execute:

``execfile("<model>_all.py")``

Where ``<model>`` is either ``fhn``, ``karma`` or ``morrislecar``.


## Directory structure and exectuing code

Below is a tree respresenting the structure of directories.

```
.
├── Bash_Copying.sh
├── Models
│   ├── FHN
│   │   └── CU
│   ├── Karma_Ramp
│   │   ├── CU
│   │   └── Nullclines
│   ├── Karma_Step
│   │   ├── CU
│   │   └── Nullclines
│   └── MorrisLecar
│       ├── CU
│       └── Nullclines
└── RawFigs
```

Generally, for each model (Fitzhugh-Nagumo, Karma and Morris-Lecar) we analyse the C-U curve, and look to the Nullclines to understand termination of the homoclinic curves. 
