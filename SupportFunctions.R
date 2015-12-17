predictOutcome <- function (mu1, mu2, sigma1, sigma2) {
  perc = CumulativeTo((mu1 - mu2) / sqrt(Beta^2 + sigma1^2 + sigma2 ^ 2))
  if (mu1 >= mu2) ans = perc
  else if (mu1 < mu2) ans = (1-perc)
  return (ans)
}

insideCumulativeTo <- function(x) {
  coeffs <- c(-1.3026537197817094, 6.4196979235649026e-1,1.9476473204185836e-2, -9.561514786808631e-3, -9.46595344482036e-4,3.66839497852761e-4, 4.2523324806907e-5, -2.0278578112534e-5,-1.624290004647e-6, 1.303655835580e-6, 1.5626441722e-8, -8.5238095915e-8,6.529054439e-9, 5.059343495e-9, -9.91364156e-10, -2.27365122e-10,9.6467911e-11, 2.394038e-12, -6.886027e-12, 8.94487e-13, 3.13092e-13,-1.12708e-13, 3.81e-16, 7.106e-15, -1.523e-15, -9.4e-17, 1.21e-16, -2.8e-17 )
  z = abs(x)
  t= 2/(2+z)
  ty = 4*t-2
  ncof = length(coeffs)
  d=0
  dd=0
  
  for (j in ncof:2) {
    tmp = d
    d = ty*d - dd + coeffs[j]
    dd = tmp
  }
  ans =2 - t*exp(-z*z + 0.5*(coeffs[1] + ty * d) - dd)
  return(ans)
}

CumulativeTo <- function(x) {
  invsqrt = -0.707106781186547524400844362104
  ans = 0.5 * insideCumulativeTo(invsqrt*x)
  return(ans)
}

NormalDistribution <- function(x) {
  mult = 1/sqrt(2 * pi)
  exppart = exp((-1 * x^2)/2)
  ans = mult * exppart
  return(ans)
}

insideInverseCumulative <- function(x) {
  if (x >= 2) ans = -100
  else if (x <= 0) ans = 100
  else {
    pp = 2-x
    t = sqrt(-2*log(pp/2))
    c = -0.70711*((2.30753 + t*0.27061)/(1+t*(0.99229 + t*0.04481)) - t)
    
    for (i in 1:2) {
      err = insideCumulativeTo(c) - pp
      c = c + err/(1.12837916709551257*exp(-1*(c^2)) - c*err)
    }
    ans = -1*c
  }
  return(ans)
}

InverseCumulative <- function(x) {
  ans = -1* sqrt(2) * insideInverseCumulative(2*x)
  return(ans)
}

GetNewMu <- function(mu1, mu2, sigma1, sigma2, result) {
  c = sqrt(2*Beta^2 + sigma1^2 + sigma2^2)
  if (result == "win") {
    newMu = mu1 + (sigma1^2 + Tau^2) / c * NormalDistribution((mu1 - mu2)/c - DrawMargin/c) / CumulativeTo((mu1-mu2)/c - DrawMargin/c)
  }
  else if (result == "loss") {
    newMu = mu1 - (sigma1^2 + Tau^2)/c * NormalDistribution((mu2 - mu1)/c - DrawMargin/c) / CumulativeTo((mu1-mu2)/c - DrawMargin/c)
  }
  else if (result == "draw") {
    t = (mu1 - mu2)/c
    UDM = DrawMargin/c
    VDen = (CumulativeTo(UDM - t) - CumulativeTo(-1 * UDM - t))
    if (VDen < 2.222758749e-162) {
      if ( c<0) {
        V= (NormalDistribution(-1*UDM - t)-NormalDistribution(UDM - t))/(-1*c - UDM)
      }
      else V= (NormalDistribution(-1*UDM - t)-NormalDistribution(UDM - t))/(-1 * c+UDM)
    }
    else {
      V = (NormalDistribution(-1*UDM - t)-NormalDistribution(UDM - t)) / VDen
    }
    newMu = mu1 + (sigma1^2 + Tau^2) / c * V
  }
  return(newMu)
}

GetNewSigma <- function(mu1, mu2, sigma1, sigma2, result) {
  c = sqrt(2*Beta^2 + sigma1^2 + sigma2^2)
  UDM = DrawMargin/c
  if (result == "win") {
    t = (mu1 - mu2)/c
    newSigma = sqrt((sigma1^2+Tau^2) *(1-(sigma1^2+Tau^2)/c^2 * NormalDistribution(t-UDM)/CumulativeTo(t-UDM) * (NormalDistribution(t-UDM)/CumulativeTo(t-UDM) + t - UDM)))
  }
  else if (result=="loss") {
    t = (mu2 - mu1)/c
    newSigma = sqrt((sigma1^2+Tau^2) *(1-(sigma1^2+Tau^2)/c^2 * NormalDistribution(t-UDM)/CumulativeTo(t-UDM) * (NormalDistribution(t-UDM)/CumulativeTo(t-UDM) + t - UDM)))
  }
  else if (result == "draw") {
    t = abs((mu1 - mu2)/c)
    VDen = (CumulativeTo(UDM - t) - CumulativeTo(-1 * UDM - t))
    if (VDen < 2.222758749e-162) {
      W=1
    }
    else {
      V = (NormalDistribution(-1*UDM - t)-NormalDistribution(UDM - t)) / VDen
      W = V^2 + ((UDM-t)*NormalDistribution(UDM-t) + (UDM + t) * NormalDistribution(UDM + t)) / (CumulativeTo(UDM-t)-CumulativeTo(-UDM-t))
    }
    newSigma = sqrt((sigma1^2+Tau^2)*(1-((sigma1^2+Tau^2)/c^2) * W))
  }
  return(newSigma)
}
