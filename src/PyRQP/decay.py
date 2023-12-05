"""
New functionality for PyRQP. In particular:
    - Decay function implemented at specific distance, alpha, beta,
      temperature and adjustment of velocity with each shot
    - Conversion of power functions to lognormal
"""

import numpy as np


def calc_time_of_travel(distance, velocity):
    """
    """
    time_of_travel = distance / velocity
    return time_of_travel


def calc_water_velocity(alpha, beta, flow_mean, flow_annual_mean):
    """
    """
    velocity = alpha * (flow_mean/flow_annual_mean) ** beta
    return velocity


def calc_adj_decay_temperature(k_20, temperature):
    """
    """
    k_t = k_20 * 1.047 ** (temperature - 20)
    return k_t


def calc_decay(c_0, decay_rate, temperature):
    """
    """
    c = c_0 * np.exp(-decay_rate * temperature)
    return e


def lognormal_to_power_function(data, mean, power_idx, base, cut_off_pc):
    """
    https://en.wikipedia.org/wiki/Power_transform
    """
    # Raise initial value to the power index
    data_p = data ** power_idx

    # TODO Make 0s all values below specified percentile

    # TODO Calculate power average
    mean_p = data_p.mean()

    # Calculate base ratio
    base_ratio = (100 - base) / 100

    # Calculate final value
    data_final = data_p * ((mean * base_ratio) / mean_p) + mean * (1 - base_ratio)
