"""
A Python implementation of the Environment Agency River Quality Planning
software
"""

import numpy as np
import pandas as pd
import rqp_utils as utils


class RiverQualityPlanner:
    """
    The main class to call RQP
    """
    # Class-level metadata properties
    random_series_size = 10000

    def __init__(self):
        # Initialize empty data structures to store river flow and water quality data
        self.river_flow_data = {}
        self.river_water_quality_data = {}
        self.discharge_flow_data = {}
        self.discharge_water_quality_data = {}
        self.correlation_data = {}
        self.results = None

    def add_river_flow_data(self, flow_data):
        # Add river flow data to the internal dict
        lg_mean, lg_sd = utils.calculate_log_mean_sd_from_95pc(
                flow_data["riv_flow_mean"], flow_data["riv_flow_95pc"]
                )
        flow_data["riv_flow_sd"] = lg_sd
        self.river_flow_data.update(flow_data)

    def add_river_water_quality_data(self, quality_data):
        # Add river water quality data to the internal dict
        self.river_water_quality_data.update(quality_data)

    def add_discharge_flow_data(self, flow_data):
        # Add discharge flow data to the internal dict
        self.discharge_flow_data.update(flow_data)

    def add_discharge_water_quality_data(self, quality_data):
        # Add discharge water quality data to the internal dict
        self.discharge_water_quality_data.update(quality_data)

    def add_correlation_data(self, correlation_data):
        # Add correlation data to the internal dict
        self.correlation_data.update(correlation_data)

    def calculate_downstream_concentration(self):
        """
        Implements the logic to predict downstream concentrations
        """
        # Correlation between river flow and quality
        pts_1 = utils.calculate_multivariate_log_normal(
                self.correlation_data["corr_riv_flow_wq"],
                self.river_flow_data["riv_flow_mean"],
                self.river_water_quality_data["riv_wq_mean"],
                self.river_flow_data["riv_flow_sd"],
                self.river_water_quality_data["riv_wq_sd"],
                )
        pts_1 = pd.DataFrame(
                pts_1, columns=["River flow", "River quality"]).sort_values("River flow")

        # Correlation between discharge flow and quality
        pts_2 = utils.calculate_multivariate_log_normal(
                self.correlation_data["corr_dis_flow_wq"],
                self.discharge_flow_data["dis_flow_mean"],
                self.discharge_water_quality_data["dis_wq_mean"],
                self.discharge_flow_data["dis_flow_sd"],
                self.discharge_water_quality_data["dis_wq_sd"],
                )
        pts_2 = pd.DataFrame(
                pts_2, columns=["Discharge flow", "Discharge quality"]
                ).sort_values("Discharge flow")

        # Correlation between river flow and discharge flow
        pts_3 = utils.calculate_multivariate_log_normal(
                self.correlation_data["corr_riv_dis_flow"],
                self.river_flow_data["riv_flow_mean"],
                self.discharge_flow_data["dis_flow_mean"],
                self.river_flow_data["riv_flow_sd"],
                self.discharge_flow_data["dis_flow_sd"],
                )
        pts_3 = pd.DataFrame(pts_3, columns=["River flow", "Discharge flow"]).sort_values(
                "River flow"
                )

        df = pd.concat(
                [pts_1, pts_3["Discharge flow"], pts_2["Discharge quality"]],
                axis=1,
                ignore_index=True,
                )
        df.columns = ["River_flow", "River_quality", "Discharge_flow", "Discharge_quality"]

        df = df.eval("Downstream_flow = River_flow + Discharge_flow")

        df = df.eval(
                "Downstream_wq = (River_flow * River_quality + Discharge_flow * Discharge_quality) / Downstream_flow"
                )

        self.results = df

    def calculate_discharge_permit(self):
        """
        Implements the logic to calculate the discharge permit
        """
        # TODO

        # Need to have initial foward calculation to have some results to set
        # the initial correlation

        # Need to transform the data depending if the target is a SD or
        # percentile concentration

        # Create random data for downstream quality that follows the desired
        # mean and sd using downstream flow mean and sd and the initial
        # correlation

        # Resolve for discharge quality and calculate stats

    def calculate_statistics(self):
        """
        Print results to screen
        """
        stats = self.results.describe().T
        stats["90pc"] = self.results.quantile(0.90)
        stats["95pc"] = self.results.quantile(0.95)
        stats["99pc"] = self.results.quantile(0.99)
        return stats

    def export_results(self, filename):
        # Export the results of predictions to a file
        # TODO
        pass

