Dim RandomNumbers(21000) As Double, ss As Long

Public Sub CalculatePCFlow()

GenerateRandomNumber

'Read in WQ data
NumWQ = Range("NWQ").value

FCounter = 0
DoCounter = 0
Dim WQData()
ReDim WQData(NumWQ)

SumFXY = 0
SumF = 0
SumWQ = 0
SumFSQR = 0
SumWQSQR = 0

Do
    DoCounter = DoCounter + 1
    If Len(Range("DataIn").Offset(DoCounter, 0).value) = 0 Then Exit Do
    If Range("DataIn").Offset(DoCounter, 1).value > 0 Then
        FCounter = FCounter + 1
        WQData(FCounter) = Range("DataIn").Offset(DoCounter, 1).value
        NewQual = Range("DataIn").Offset(DoCounter, 1).value
        NFlow = Range("DataIn").Offset(DoCounter, 2).value
        
        SumFXY = SumFXY + NFlow * NewQual
        SumF = SumF + NFlow
        SumWQ = SumWQ + NewQual
        SumFSQR = SumFSQR + (NFlow ^ 2)
        SumWQSQR = SumWQSQR + (NewQual ^ 2)
    End If
Loop

NumHist = FCounter

CORWQF = ((NumHist * SumFXY) - (SumF * SumWQ)) / (((NumHist * SumFSQR) - (SumF ^ 2)) * ((NumHist * SumWQSQR) - (SumWQ ^ 2))) ^ 0.5

Range("MQ").Offset(2, 0).value = CORWQF


Call QuickSort(WQData, LBound(WQData), UBound(WQData))


'Read in a sort reference flow
Dim ReferenceFlow() As Double, ss As Long, NewValue As Double

RefCount = Range("FCount").value

ReDim ReferenceFlow(RefCount)

DoCounter = 0

Do
    DoCounter = DoCounter + 1
    If Len(Range("HistFlow").Offset(DoCounter, 0).value) = 0 Then Exit Do
    ReferenceFlow(DoCounter) = Range("HistFlow").Offset(DoCounter, 1).value
Loop

Call QuickSort(ReferenceFlow, LBound(ReferenceFlow), UBound(ReferenceFlow))

Dim FRandDeviate As Double, CNewValue As Double, MeanQual As Double, SDQual As Double
Dim FPosition As Double, NewRnd As Double, CORWQF2 As Double

DoCounter = 0
SumFXY = 0
SumF = 0
SumWQ = 0
SumFSQR = 0
SumWQSQR = 0


MeanQual = Range("MQ").value
SDQual = Range("MQ").Offset(1, 0).value
CORWQF2 = Range("MQ").Offset(2, 0).value

Do
    DoCounter = DoCounter + 1
    If Len(Range("SFlow").Offset(DoCounter, 0).value) = 0 Then Exit Do
    FlowValue = Range("SFlow").Offset(DoCounter, 1).value
    For pp = 1 To RefCount - 1
        If FlowValue >= ReferenceFlow(pp) And FlowValue < ReferenceFlow(pp + 1) Then
            Range("SFlow").Offset(DoCounter, 2).value = 100 * pp / RefCount
        End If
    Next pp
    
    CurrentPC = Range("SFlow").Offset(DoCounter, 2).value
    FPosition = CurrentPC / 100
    ss = DoCounter
    
    If Range("DType").value = "Log Normal" Then
        Call Dist(0, 1, FRandDeviate, FPosition, 0, ss)
        Call LogNormalRandDeviate(MeanQual, SDQual, FRandDeviate, CORWQF2, 109, NewValue, 0, 0, ss)
        Range("SFlow").Offset(DoCounter, 3).value = NewValue
    End If
    
    If Range("DType").value = "Normal" Then
        Call Dist(0, 1, FRandDeviate, FPosition, 0, ss)
        Call NormalRandDeviate(MeanQual, SDQual, FRandDeviate, CORWQF2, 109, NewValue, 0, 0, ss)
        Range("SFlow").Offset(DoCounter, 3).value = NewValue
    End If
    
     If Range("DType").value = "Non parametric" Then
        Call Dist(0, 1, FRandDeviate, FPosition, 0, ss)
        Call NormalRandDeviate(0, 1, FRandDeviate, CORWQF2, 109, NewValue, 0, 0, ss)
        Call NewRndValue(NewValue, NewRnd)
        
        Location = Int(NewRnd * NumWQ)
        NV = WQData(Location)
        Range("SFlow").Offset(DoCounter, 3).value = NV
    End If
    
    NewQual = Range("SFlow").Offset(DoCounter, 3).value
    NFlow = FlowValue
    SumFXY = SumFXY + NFlow * NewQual
    SumF = SumF + NFlow
    SumWQ = SumWQ + NewQual
    SumFSQR = SumFSQR + (NFlow ^ 2)
    SumWQSQR = SumWQSQR + (NewQual ^ 2)
Loop

NumHist = DoCounter - 1
CORWQF = ((NumHist * SumFXY) - (SumF * SumWQ)) / (((NumHist * SumFSQR) - (SumF ^ 2)) * ((NumHist * SumWQSQR) - (SumWQ ^ 2))) ^ 0.5

Range("OC").value = CORWQF

End Sub


Public Sub LogNormalRandDeviate(Mean As Double, SD As Double, FixedDischargeDeviate As Double, DischargeCorr As Double, Seed As Long, NewValue As Double, FixedTempDeviate As Double, TempCorr As Double, Nj As Long)
'Works out log normal random deviate based on specidied mean and standar deviation (based on library code)

Static iset, gset

If gsetcounter = 0 Then gset = Empty
gsetcounter = gsetcounter + 1

'Generate log normal random deviate
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
End Sub

Public Sub NormalRandDeviate(Mean As Double, SD As Double, FixedDischargeDeviate As Double, DischargeCorr As Double, Seed As Long, NewValue As Double, FixedTempDeviate As Double, TempCorr As Double, Nj As Long)
'Works out log normal random deviate based on specidied mean and standar deviation (based on library code)

Static iset, gset

If gsetcounter = 0 Then gset = Empty
gsetcounter = gsetcounter + 1

'Generate log normal randam deviate
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

End Sub

Public Sub Dist(Mean As Double, SD As Double, value As Double, DisScale As Double, Corr As Double, nx As Long)
'Generates a value between 0 and 1 based on random deviate and correlation factors with another random number

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

End Sub

Public Sub NewRndValue(UPMDeviate As Double, NewRnd As Double)
'Generates a value between 0 and 1 based on random deviate

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
End Sub

" Public Sub GenerateRandomNumber()

" 'Generates an array of 21000 random mumbers
" Dim gg As Long, GDRan, iset As Integer, gset As Double, gsetcounter As Long

" iset = 0
" gset = 0

" RandomNumbers(1) = Rnd(-1)
" gsetcounter = 0

" For gg = 1 To 21000
"     RandomNumbers(gg) = Rnd(200)
" Next gg

" End Sub

" Public Sub QuickSort(vArray As Variant, inLow As Long, inHi As Long)
"   Dim pivot   As Variant
"   Dim tmpSwap As Variant
"   Dim tmpLow  As Long
"   Dim tmpHi   As Long

"   tmpLow = inLow
"   tmpHi = inHi

"   pivot = vArray((inLow + inHi) \ 2)

"   While (tmpLow <= tmpHi)
"      While (vArray(tmpLow) < pivot And tmpLow < inHi)
"         tmpLow = tmpLow + 1
"      Wend

"      While (pivot < vArray(tmpHi) And tmpHi > inLow)
"         tmpHi = tmpHi - 1
"      Wend

"      If (tmpLow <= tmpHi) Then
"         tmpSwap = vArray(tmpLow)
"         vArray(tmpLow) = vArray(tmpHi)
"         vArray(tmpHi) = tmpSwap
"         tmpLow = tmpLow + 1
"         tmpHi = tmpHi - 1
"      End If
"   Wend

"   If (inLow < tmpHi) Then QuickSort vArray, inLow, tmpHi
"   If (tmpLow < inHi) Then QuickSort vArray, tmpLow, inHi

" End Sub
