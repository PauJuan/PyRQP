# PyRQP

An open source version of the Environment Agency's River Quality Planning (RQP) software written in Python. It aims to be transparent, extendable and easy to automate.

[comment]: <TODO Add link to documentation>

## RQP

RQP is one of the main tools used for discharge permitting and water management in the UK. The main purpose of the tool is to simulate the mixing of two inputs (generally a river and a discharge) to estimate the distribution of the output concentration of the downstream mixing zone. Typical uses of the tool would be to determine the possible impact of an increased flow discharged at a location, or the quality required at a given discharge to meet a specific water quality standard downstream of the discharge. RQP is the model underpinning the functionality implemented by the [Environment Agency](https://www.gov.uk/government/organisations/environment-agency)'s catchment simulator SIMCAT and [UKWIR](https://ukwir.org/)'s [SAGIS](https://sagis.ukwir.org/sagis/welcome).

There are two key points underlying the methodology of the RQP tool:
1.	The input data are correlated. That is, they are dependent on one another and change together to a certain degree.
2.	It is possible to simulate the mix of two distributions by using a numerical method such as Monte Carlo (i.e., repeated random sampling to obtain numerical results).

### Correlation

RQP generates four sets of random correlated data based on each parameter's distributions, which are generally considered to be lognormal. The parameters are: river flow, discharge flow, river concentration and discharge concentration. The following is a graphical representation of what this looks like for river flow and discharge flow. The correlation between this two variables is typically 0.6.

![flow_correlation](https://github.com/PauJuan/PyRQP/blob/main/docs/img/flow_correlation.png?raw=true)

*Example of river flow and discharge flow correlation of 0.6 for two lognormal distributions. River flow mean and std are 20 and 4.5; discharge flow mean and std are 4 and 0.9. These values are approximate*

Complementary, the correlation between river flow and quality is typically -0.3; an example follows.

![flow_wq_correlation](https://github.com/PauJuan/PyRQP/blob/main/docs/img/flow_wq_correlation.png?raw=true)

*Example of river flow and quality correlation of -0.3 for two lognormal distributions.*

### Monte Carlo

By applying a simple mass balance, the downstream flow and concentration is calculated. A graphical representation of the Monte Carlo method for mixing distributions is shown below.

![MonteCarlo](https://github.com/PauJuan/PyRQP/blob/main/docs/img/RQP_method.png?raw=true)

*Monte Carlo sampling method*

> Source: Henderson, Brent & Bui, Elisabeth. (2005). Determining Uncertainty in Sediment & Nutrient Transport Models for Ecological Risk Assessment. Report No 2. LWA/MDBC Project UMO43: Risk-based Approaches to Managing Contaminants in Catchments. CSIRO.

## Usage

Please check the notebook folder for examples of how to use it.

- [Proof of concept](https://github.com/PauJuan/PyRQP/blob/main/notebooks/pyrqp_poc.ipynb). A simple notebook that contains the bare bones of the methodology to show how RQP works
- [Simple example](https://github.com/PauJuan/PyRQP/blob/main/notebooks/rqp_simple_example.ipynb). An example of using the PyRQP library for one case study (forward and backward calculation)
- [Batch calculation example](https://github.com/PauJuan/PyRQP/blob/main/notebooks/rqp_batch_example.ipynb). An example of using the PyRQP library to undertake RQP calculations in batches from an Excel file

## Installation

This package is available for installation using [PyPI](https://pypi.org/project/PyRQP/):

    pip install PyRQP

However, the pip version may lag behind the release version. For most users it is recommended that they download the package directly from GitHub.

## Citation

Please consider citing this website when using PyRQP:

> Juan-Garcia, P. (2023, October 17). PauJuan/PyRQP Open source River Quality Planning software written in Python. GitHub. https://github.com/PauJuan/PyRQP

## License

Copyright (C) 2023 [Pau Juan-Garcia](https://paujuan.github.io/)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
