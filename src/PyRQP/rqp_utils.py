"""
A compendium of equations used by the RQP tool
"""

import numpy as np

def calculate_log_mean_sd_from_95pc(lg_mean, lg_95pc):
    """
    """
    sd = (2.705543 + 2*np.log(lg_mean) - 2*np.log(lg_95pc)) ** 0.5 - 1.644854
    mean = np.log(lg_mean) - 0.5 * (sd**0.5)
    lg_sd = lg_mean * (np.exp(sd**2) - 1) ** 0.5
    return lg_mean, lg_sd

def transform_log_to_normal(lg_mean, lg_sd):
    """
    """
    mean = np.log(lg_mean / ((1 + ((lg_sd**2) / (lg_mean**2))) ** 0.5))
    sd = (np.log(1 + (lg_sd**2) / (lg_mean**2))) ** 0.5
    return mean, sd


def calculate_covariance(corr, std_1, std_2):
    """
    This formula takes a correlation and two std
    and calculates the covariance matrix
    """
    x = corr * np.sqrt(std_1**2 * std_2**2)
    cov = [[std_1**2, x], [x, std_2**2]]
    return cov


def calculate_multivariate_normal(mean_1, mean_2, cov):
    """
    This formula takes a covariation matrix and two
    mean values and calculates a two random series of
    multivariate variables of the specified size
    """
    data = np.random.multivariate_normal([mean_1, mean_2], cov, size=10000)
    return data


def calculate_multivariate_log_normal(corr, mean_1, mean_2, std_1, std_2):
    """
    This function calculates two series of random corelated data
    for two variables using their mean, std and the desired correlation
    """
    # Transform to 'normal' statistical moments
    mean_1, std_1 = transform_log_to_normal(mean_1, std_1)
    mean_2, std_2 = transform_log_to_normal(mean_2, std_2)
    # Calculate covariance
    cov = calculate_covariance(corr, std_1, std_2)
    # Calcualate random multivariate data
    pts = calculate_multivariate_normal(mean_1, mean_2, cov)
    # Transform to lognormal
    pts = np.exp(pts)
    return pts
