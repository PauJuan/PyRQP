"""
A Python implementation of the Environment Agency River Quality Planning
software
"""

import numpy as np
import pandas as pd
import utils


class RiverQualityPlanner:
    """
    The main class to call RQP
    """
    # Class-level metadata properties
    random_series_size = 100000
    random_series_seed = 42
    np.random.seed(42)

    # Target properties
    target_value = None
    target_pc = None

    def __init__(self):
        # Initialize empty data structures to store river flow and water quality data
        self.rqp_data = {}
        self.results = None
        self.stats = None

    def set_random_params(self, size=100000, seed=42):
        # Set bespoke values for random parameters
        self.random_series_seed = seed
        self.random_series_size = size
        np.random.seed(self.random_series_seed)

    def set_target(self, value, percentile):
        # Set target for back calculation
        self.target_value = value
        self.target_pc = percentile

    def add_rqp_data(self, rqp_data):
        # Add river flow data to the internal dict
        lg_mean, lg_sd = utils.calculate_log_mean_sd_from_95pc(
                rqp_data["riv_flow_mean"], rqp_data["riv_flow_95pc"]
                )
        rqp_data["riv_flow_sd"] = lg_sd
        self.rqp_data.update(rqp_data)

    def calculate_downstream_concentration(self):
        """
        Implements the logic to predict downstream concentrations
        """
        # Run main function to generate random data
        df = utils.calculate_multivariate_log_normal(
                # Flow
                mean1=self.rqp_data["riv_flow_mean"],
                std1=self.rqp_data["riv_flow_sd"],
                mean2=self.rqp_data["dis_flow_mean"],
                std2=self.rqp_data["dis_flow_sd"],
                # Quality
                mean3=self.rqp_data["riv_wq_mean"],
                std3=self.rqp_data["riv_wq_sd"],
                mean4=self.rqp_data["dis_wq_mean"],
                std4=self.rqp_data["dis_wq_sd"],
                # Correlation
                corr1_2=self.rqp_data["corr_riv_dis_flow"],
                corr1_3=self.rqp_data["corr_riv_flow_wq"],
                corr2_4=self.rqp_data["corr_dis_flow_wq"],
                random_size=self.random_series_size
                )

        # Calculate downstream flow and quality
        df = df.eval("ds_flow = riv_flow + dis_flow")
        df = df.eval("ds_qual = (riv_flow * riv_qual + dis_flow * dis_qual) / ds_flow")

        self.results = df
        self.update_stats()

    def calculate_discharge_permit(self):
        """
        Implements the logic to calculate the discharge permit
        """
        df = self.results
        target = self.target_value
        percentile = self.target_pc

        # Calculate adjustment factors and scale distribution
        df["ds_qual_target"] = df["ds_qual"]
        scale = target / df["ds_qual"].quantile(percentile)

        while not (0.9999 <= scale <= 1.0001):

            # Scale target distribution
            df["ds_qual_target"] = df["ds_qual_target"] * scale

            # Recalculate discharge quality target
            df = df.eval("dis_qual_target = (ds_flow * ds_qual_target - riv_flow * riv_qual) / dis_flow")

            # Recalculate discharge quality keeping CoV
            adj_factor = df["dis_qual_target"].mean() / df["dis_qual"].mean()
            df["dis_qual_target"] = df["dis_qual"] * adj_factor

            # Recalculate downstream water quality and check scale
            df = df.eval("ds_qual_target = (riv_flow * riv_qual + dis_flow * dis_qual_target) / ds_flow")
            scale = target / df["ds_qual_target"].quantile(percentile)

        # Overwrite results
        self.results = df
        self.update_stats()

    def update_stats(self):
        """
        Calculate summary statistics
        """
        df = self.results
        stats = df.agg(["mean", "std"]).T
        stats["90pc"] = df.quantile(0.90)
        stats["95pc"] = df.quantile(0.95)
        stats["99pc"] = df.quantile(0.99)
        stats["99.5pc"] = df.quantile(0.995)
        stats["cov"] = stats["std"] / stats["mean"]
        self.stats = stats

    def get_stats(self):
        """
        Commodity function to retrieve statistics
        """
        df = self.stats
        return df


    def export_results(self, filename):
        """
        Commodity function to export statistics to excel
        """
        df = self.stats
        df.to_excel(filename)
