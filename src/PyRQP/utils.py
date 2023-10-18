"""
A compendium of equations used by the RQP tool
"""

import numpy as np
import pandas as pd


def calculate_log_mean_sd_from_95pc(lg_mean, lg_95pc):
    """
    Calculate the underlying normal sd from the lognormal
    mean and 95th low flow percentile
    """
    sd = (2.705543 + 2*np.log(lg_mean) - 2*np.log(lg_95pc)) ** 0.5 - 1.644854
    mean = np.log(lg_mean) - 0.5 * (sd**0.5)
    lg_sd = lg_mean * (np.exp(sd**2) - 1) ** 0.5
    return lg_mean, lg_sd


def transform_log_to_normal(lg_mean, lg_sd):
    """
    Transformation from log mean and sd to normal
    mean and sd using the method of moments
    """
    mean = np.log(lg_mean / ((1 + ((lg_sd**2) / (lg_mean**2))) ** 0.5))
    sd = (np.log(1 + (lg_sd**2) / (lg_mean**2))) ** 0.5
    return mean, sd


def calculate_covariance(corr, std_1, std_2):
    """
    This formula takes a correlation and two std
    and calculates the covariance
    """
    cov = corr * std_1 * std_2
    return cov


def calculate_multivariate_log_normal(
        mean1, std1, mean2, std2, mean3, std3, mean4, std4, corr1_2, corr1_3,
        corr2_4, random_size
        ):
    """
    The main equation in RQP to generate the lognormal
    random multivariate dataset
    """
    # Transform parameters to normal
    mean1, std1 = transform_log_to_normal(mean1, std1)
    mean2, std2 = transform_log_to_normal(mean2, std2)
    mean3, std3 = transform_log_to_normal(mean3, std3)
    mean4, std4 = transform_log_to_normal(mean4, std4)

    # Calculate covariances
    cov1_2 = calculate_covariance(corr1_2, std1, std2)
    cov1_3 = calculate_covariance(corr1_3, std1, std3)
    cov2_4 = calculate_covariance(corr2_4, std2, std4)

    # Build covariance matrix
    cov_matrix = [
            [std1**2, cov1_2, cov1_3, 0],
            [cov1_2, std2**2, 0, cov2_4],
            [cov1_3, 0, std3**2, 0],
            [0, cov2_4, 0, std4**2],
            ]
    cov_matrix = np.array(cov_matrix)

    # Generate normal random multivariate
    data = np.random.multivariate_normal(
            [mean1, mean2, mean3, mean4], cov_matrix, size=random_size
            )

    # Transform to lognormal and build dataframe
    data = np.exp(data)
    df = pd.DataFrame(data, columns=["riv_flow", "dis_flow", "riv_qual", "dis_qual"])

    return df
