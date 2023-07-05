

load(file = 'com.no.dist.Rdata')
load(file= 'com.no.dist.sp.Rdata')
load(file= 'com.no.dist.rgrH.Rdata')
load(file= 'com.no.dist.rgrH.sp.Rdata')

com.no.dist.ce <- conditional_effects(com.no.dist)
com.no.dist.df <- as.data.frame(com.no.dist.ce$Treatment)

com.no.dist.sp.ce <- conditional_effects(com.no.dist.sp)
com.no.dist.sp.df <- as.data.frame(com.no.dist.sp.ce$Treatment)

com.no.dist.rgrH.ce <- conditional_effects(com.no.dist.rgrH)
com.no.dist.rgrH.df <- as.data.frame(com.no.dist.rgrH.ce$Treatment)

com.no.dist.rgrH.sp.ce <- conditional_effects(com.no.dist.rgrH.sp)
com.no.dist.rgrH.sp.df <- as.data.frame(com.no.dist.rgrH.sp.ce$sci.name)
