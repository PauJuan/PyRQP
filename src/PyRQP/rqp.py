# TODO
"""
Create an incredible PyRQP library to do RQP using normal, log-normal, power
function and non-parametric distributions

Use solutions from link below (be modern)

Create the repository and associated documentation

Create notebooks as examples and make a blog post about it

Show Carlos and Peter and discuss

The next step after having a working library, apart from documentation etc.
would be to implement the possibility to calculate decay at a specific reach
length. For this we can use the velocity equation, the temperature adjusted
decay equation, and of course the decay calculation.
"""

import numpy as np

# https://stackoverflow.com/questions/27727762/scipy-generate-random-variables-with-correlations

# https://stackoverflow.com/questions/16024677/generate-correlated-data-in-python-3-3/16025584#16025584

# https://realpython.com/numpy-scipy-pandas-correlation-python/

# https://stats.stackexchange.com/questions/83172/generate-two-variables-with-precise-pre-specified-correlation

# https://numpy.org/doc/stable/reference/random/generated/numpy.random.multivariate_normal.html

# https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.multivariate_normal.html

# https://en.wikipedia.org/wiki/Power_transform

# https://scikit-learn.org/stable/auto_examples/preprocessing/plot_map_data_to_normal.html

# https://stackoverflow.com/questions/41109122/fitting-a-curve-to-a-power-law-distribution-with-curve-fit-does-not-work

# https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.fit.html

# https://math.stackexchange.com/questions/2919631/finding-correlation-coefficient-from-covariance-matrix

# https://www.investopedia.com/terms/c/covariance.asp#:~:text=Covariance%20is%20calculated%20by%20analyzing,standard%20deviation%20of%20each%20variable

random_numbers =  # create variable with 21,000 random numbers


def calculate_perc_flow(random_numbers):
    """
    """
    # Read In WQ and flow data

    # Calculate correlation between the two

    # Sort WQ data

    # Sort 'reference' flow data. ASK PETER FOR DETAILS

    # Read in desired mean, SD and the calculated correlation

    # Calculate column with flow exceedance

    # Calculate random deviate depending on equation needed (normal, log normal
    # or non-parametric) and new flows

    # Log new mean, SD and correlation of broadcasted dataset


def log_normal_random_deviate(Mean As Double, SD As Double,
                              FixedDischargeDeviate As Double, DischargeCorr As
                              Double, Seed As Long, NewValue As Double,
                              FixedTempDeviate As Double, TempCorr As Double, Nj
                              As Long):
    """
    Works out log normal random deviate based on specified mean and standard deviation (based on library code)
    """
    # Generate log normal random deviate
    LGST = (Log(1 + (SD ^ 2) / (Mean ^ 2))) ^ 0.5
    LGMean = Log(Mean / ((1 + ((SD ^ 2) / (Mean ^ 2))) ^ 0.5))

    XRand = Nj

    If XRand > 20000 Then
    XRand = XRand - (Int((XRand / 20000)) * 20000)
    End If

    idum = 200

    If idum < 0 Then iset = 0
    If iset = 0 Then
      DoRndCounter = 0
      Do
        DoRndCounter = DoRndCounter + 1
        V1 = 2 * RandomNumbers(XRand + DoRndCounter) - 1
        V2 = 2 * RandomNumbers(XRand + DoRndCounter + 1) - 1
        RSq = V1 ^ 2 + V2 ^ 2
        If RSq > 1 Or RSq = 0 Then
        Else
          Exit Do
        End If
      Loop

      fac = (-2 * Log(RSq) / RSq) ^ 0.5
      gset = V1 * fac
      gasdev = V2 * fac
      iset = 1
    Else

      gasdev = gset
      iset = 0
    End If

    b1 = TempCorr
    b2 = (1 - b1 ^ 2) ^ 0.5
    c1 = DischargeCorr
    c2 = (TempCorr - b1 * c1) / b2
    c3 = (1 - (c1 ^ 2) - (c2 ^ 2)) ^ 0.5
    M = c1 * FixedDischargeDeviate + c2 * FixedTempDeviate + c3 * gasdev
    value = (M * LGST) + LGMean
    NewValue = Exp(value)

    return new_value


def NormalRandDeviate(Mean As Double, SD As Double, FixedDischargeDeviate As
                      Double, DischargeCorr As Double, Seed As Long, NewValue As
                      Double, FixedTempDeviate As Double, TempCorr As Double, Nj
                      As Long):
    """
    Works out log normal random deviate based on specidied mean and standar deviation (based on library code)
    """

    # Generate log normal random deviate
    LGST = SD
    LGMean = Mean

    XRand = Nj

    If XRand > 20000 Then
    XRand = XRand - (Int((XRand / 20000)) * 20000)
    End If

    idum = 200

    If idum < 0 Then iset = 0
    If iset = 0 Then
      DoRndCounter = 0
      Do
        DoRndCounter = DoRndCounter + 1
        V1 = 2 * RandomNumbers(XRand + DoRndCounter) - 1
        V2 = 2 * RandomNumbers(XRand + DoRndCounter + 1) - 1
        RSq = V1 ^ 2 + V2 ^ 2
        If RSq > 1 Or RSq = 0 Then
        Else
          Exit Do
        End If
      Loop

      fac = (-2 * Log(RSq) / RSq) ^ 0.5
      gset = V1 * fac
      gasdev = V2 * fac
      iset = 1
    Else

      gasdev = gset
      iset = 0
    End If

    b1 = GlobalTempDischargeCorr
    b2 = (1 - b1 ^ 2) ^ 0.5
    c1 = DischargeCorr
    c2 = (TempCorr - b1 * c1) / b2
    c3 = (1 - (c1 ^ 2) - (c2 ^ 2)) ^ 0.5
    M = c1 * FixedDischargeDeviate + c2 * FixedTempDeviate + c3 * gasdev
    NewValue = (M * SD) + Mean

    return new_value


def dist(Mean As Double, SD As Double, value As Double, DisScale As Double, Corr
         As Double, nx As Long):
    """
    Generates a value between 0 and 1 based on random deviate and correlation factors with another random number
    """

    Dim y0 As Double, y1 As Double, areaz As Double, filenum

    Z = -3

    XRand = Rnd
    If XRand > 10000 Then XRand = XRand - (Int((XRand / 10000)) * 10000)

    X = DisScale
    step1 = 0.025
    xx = 1 / ((2 * 3.14159265358979) ^ 0.5)
    Counter = 0
    cumarea = 0.001349967

    Do
      oldcumarea = cumarea
      z1 = Z + step1
      y0 = xx * Exp(-(Z ^ 2) / 2)
      y1 = xx * Exp(-(z1 ^ 2) / 2)
      areaz = step1 * ((y0 + y1) / 2)
      cumarea = cumarea + areaz
      Counter = Counter + 1
      If Counter > 1000 Then Exit Do
       If cumarea > X Then Exit Do
      Z = Z + step1
    Loop

    If Counter <= 1000 Then interpolz = Z + ((X - oldcumarea) / (cumarea - oldcumarea)) * step1

    If Corr <> 0 Then
      zn = -3
      xn = DisScale
      step1n = 0.025
      xxn = 1 / ((2 * 3.14159265358979) ^ 0.5)
      Countern = 0
      cumarean = 0.001349967

      Do
        oldcumarean = cumarean
        z1n = zn + step1n
        y0n = xxn * Exp(-(zn ^ 2) / 2)
        y1n = xxn * Exp(-(z1n ^ 2) / 2)
        areazn = step1n * ((y0n + y1n) / 2)
         cumarean = cumarean + areazn
        Countern = Countern + 1
        If Countern > 1000 Then
          Exit Do
        End If

        If cumarean > xn Then Exit Do
        zn = zn + step1n
      Loop

      If Countern <= 1000 Then interpolzn = zn + ((xn - oldcumarean) / (cumarean - oldcumarean)) * step1n
    End If

    M = (interpolzn * Corr) + interpolz * ((1 - Corr ^ 2) ^ 0.5)

    value = (M * SD) + Mean

    return value


def NewRndValue(UPMDeviate As Double, NewRnd As Double):
    """
    Generates a value between 0 and 1 based on random deviate
    """
    Z = -3
    step1 = 0.01
    xx = 1 / ((2 * 3.14159265358979) ^ 0.5)
    Counter = 0
    cumarea = 0.001349967

    Do
        oldcumarea = cumarea
        z1 = Z + step1
        y0 = xx * Exp(-(Z ^ 2) / 2)
        y1 = xx * Exp(-(z1 ^ 2) / 2)
        areaz = step1 * ((y0 + y1) / 2)
        cumarea = cumarea + areaz
        Counter = Counter + 1
        If Counter > 1000 Then Exit Do
        If z1 > UPMDeviate Then Exit Do
        Z = Z + step1
    Loop

    NewRnd = cumarea

    return new_random
