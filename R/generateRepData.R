
generateRepData <- function(nofg = 20, tpl = 1, tpu = 6, unequal = FALSE, nofgl = NULL, nofgu = NULL) {

	if(unequal == FALSE){
		rep1 <- mat.or.vec(nofg*15, 6)
		rep2 <- mat.or.vec(nofg*15, 6)

		for(i in 1:nofg){
			rep1[i,1]=runif(1,2,4)
			rep1[i,2]=rep1[i,1]+runif(1,-0.2,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,-0.2,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,-0.2,0.2)
			rep1[i,5]=rep1[i,4]+runif(1,-0.2,0.2)
			rep1[i,6]=rep1[i,5]+runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,2,4)
			rep2[i,2]=rep2[i,1]+runif(1,-0.2,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,-0.2,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,-0.2,0.2)
			rep2[i,5]=rep2[i,4]+runif(1,-0.2,0.2)
			rep2[i,6]=rep2[i,5]+runif(1,-0.2,0.2)}

		for(i in (nofg+1):(2*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0.2,0.6)
			rep1[i,4]=rep1[i,3]+runif(1,0.6,1.2)
			rep1[i,5]=rep1[i,4]+runif(1,1.2,2)
			rep1[i,6]=rep1[i,5]+runif(1,2,3)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,0,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,0.2,0.6)
			rep2[i,4]=rep2[i,3]+runif(1,0.6,1.2)
			rep2[i,5]=rep2[i,4]+runif(1,1.2,2)
			rep2[i,6]=rep2[i,5]+runif(1,2,3)}

		for(i in (2*nofg+1):(3*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,0.4,0.8)
			rep1[i,5]=rep1[i,4]+runif(1,0.4,0.8)
			rep1[i,6]=rep1[i,5]+runif(1,1,1.2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,0,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,0,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,0.4,0.8)
			rep2[i,5]=rep2[i,4]+runif(1,0.4,0.8)
			rep2[i,6]=rep2[i,5]+runif(1,1,1.2)}

		for(i in (3*nofg+1):(4*nofg)){
			rep1[i,1]=runif(1,4,6)
			rep1[i,2]=rep1[i,1]+runif(1,-0.2,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,-0.2,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]+runif(1,2,4)
			rep1[i,6]=rep1[i,5]+runif(1,1,2)

			rep2[i,1]=runif(1,4,6)
			rep2[i,2]=rep2[i,1]+runif(1,-0.2,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,-0.2,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]+runif(1,2,4)
			rep2[i,6]=rep2[i,5]+runif(1,1,2)}

		for(i in (4*nofg+1):(5*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,8,10)
			rep1[i,3]=rep1[i,2]-runif(1,5,7)
			rep1[i,4]=rep1[i,3]-runif(1,1,2)
			rep1[i,5]=rep1[i,4]-runif(1,0.5,1)
			rep1[i,6]=rep1[i,5]-runif(1,1,2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,8,10)
			rep2[i,3]=rep2[i,2]-runif(1,5,7)
			rep2[i,4]=rep2[i,3]-runif(1,1,2)
			rep2[i,5]=rep2[i,4]-runif(1,0.5,1)
			rep2[i,6]=rep2[i,5]-runif(1,1,2)}

		for(i in (5*nofg+1):(6*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,5,7)
			rep1[i,3]=rep1[i,2]+runif(1,2,4)
			rep1[i,4]=rep1[i,3]-runif(1,2,4)
			rep1[i,5]=rep1[i,4]-runif(1,1.2,2)
			rep1[i,6]=rep1[i,5]-runif(1,2,3)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,5,7)
			rep2[i,3]=rep2[i,2]+runif(1,2,4)
			rep2[i,4]=rep2[i,3]-runif(1,2,4)
			rep2[i,5]=rep2[i,4]-runif(1,1.2,2)
			rep2[i,6]=rep2[i,5]-runif(1,2,3)}

		for(i in (6*nofg+1):(7*nofg)){
			rep1[i,1]=runif(1,2,4)
			rep1[i,2]=rep1[i,1]+runif(1,1,2)
			rep1[i,3]=rep1[i,2]+runif(1,1,2)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]-runif(1,2,4)
			rep1[i,6]=rep1[i,5]-runif(1,1,2)

			rep2[i,1]=runif(1,2,4)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,1,2)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]-runif(1,2,4)
			rep2[i,6]=rep2[i,5]-runif(1,1,2)}

		for(i in (7*nofg+1):(8*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,1,2)
			rep1[i,3]=rep1[i,2]+runif(1,2,3)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]+runif(1,2,3)
			rep1[i,6]=rep1[i,5]-runif(1,4,5)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,2,3)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]+runif(1,2,3)
			rep2[i,6]=rep2[i,5]-runif(1,4,5)}

		for(i in (8*nofg+1):(9*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,5,7)
			rep1[i,3]=rep1[i,2]+runif(1,2,4)
			rep1[i,4]=rep1[i,3]-runif(1,1,3)
			rep1[i,5]=rep1[i,4]+runif(1,-0.2,0.2)
			rep1[i,6]=rep1[i,5]+runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,5,7)
			rep2[i,3]=rep2[i,2]+runif(1,2,4)
			rep2[i,4]=rep2[i,3]-runif(1,1,3)
			rep2[i,5]=rep2[i,4]+runif(1,-0.2,0.2)
			rep2[i,6]=rep2[i,5]+runif(1,-0.2,0.2)}

		for(i in (9*nofg+1):(10*nofg)){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]-runif(1,4,6)
			rep1[i,3]=rep1[i,2]-runif(1,2,2.5)
			rep1[i,4]=rep1[i,3]+runif(1,3,5)
			rep1[i,5]=rep1[i,4]+runif(1,1,3)
			rep1[i,6]=rep1[i,5]+runif(1,1,3)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]-runif(1,4,6)
			rep2[i,3]=rep2[i,2]-runif(1,2,2.5)
			rep2[i,4]=rep2[i,3]+runif(1,3,5)
			rep2[i,5]=rep2[i,4]+runif(1,1,3)
			rep2[i,6]=rep2[i,5]+runif(1,1,3)}

		for(i in (10*nofg+1):(11*nofg)){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]+runif(1,3,5)
			rep1[i,3]=rep1[i,2]-runif(1,2,4)
			rep1[i,4]=rep1[i,3]-runif(1,3,5)
			rep1[i,5]=rep1[i,4]-runif(1,1,2)
			rep1[i,6]=rep1[i,5]+runif(1,2,3)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]+runif(1,3,5)
			rep2[i,3]=rep2[i,2]-runif(1,2,4)
			rep2[i,4]=rep2[i,3]-runif(1,3,5)
			rep2[i,5]=rep2[i,4]-runif(1,1,2)
			rep2[i,6]=rep2[i,5]+runif(1,2,3)}

		for(i in (11*nofg+1):(12*nofg)){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]-runif(1,4,6)
			rep1[i,3]=rep1[i,2]+runif(1,4,6)
			rep1[i,4]=rep1[i,3]+runif(1,-0.2,0.2)
			rep1[i,5]=rep1[i,4]+runif(1,-0.2,0.2)
			rep1[i,6]=rep1[i,5]+runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]-runif(1,4,6)
			rep2[i,3]=rep2[i,2]+runif(1,4,6)
			rep2[i,4]=rep2[i,3]+runif(1,-0.2,0.2)
			rep2[i,5]=rep2[i,4]+runif(1,-0.2,0.2)
			rep2[i,6]=rep2[i,5]+runif(1,-0.2,0.2)}

		for(i in (12*nofg+1):(13*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0.2,0.6)
			rep1[i,4]=rep1[i,3]+runif(1,0.6,1.2)
			rep1[i,5]=rep1[i,4]+runif(1,1.2,2)
			rep1[i,6]=rep1[i,5]+runif(1,2,3)

			rep2[i,1]=runif(1,2,4)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,1,2)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]-runif(1,2,4)
			rep2[i,6]=rep2[i,5]-runif(1,1,2)}

		for(i in (13*nofg+1):(14*nofg)){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,1,2)
			rep1[i,3]=rep1[i,2]+runif(1,2,3)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]+runif(1,2,3)
			rep1[i,6]=rep1[i,5]-runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,2,3)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]+runif(1,2,3)
			rep2[i,6]=rep2[i,5]-runif(1,-0.2,0.2)}

		for(i in (14*nofg+1):(15*nofg)){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,0.4,0.8)
			rep1[i,5]=rep1[i,4]+runif(1,0.4,0.8)
			rep1[i,6]=rep1[i,5]+runif(1,1,1.2)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]+runif(1,0,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,0,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,0.4,0.8)
			rep2[i,5]=rep2[i,4]+runif(1,0.4,0.8)
			rep2[i,6]=rep2[i,5]+runif(1,1,1.2)}

		data <- cbind(rep1[, tpl:tpu], rep2[, tpl:tpu])
		groups <- rep(1:15, each = nofg)
		dataframe <- list(nofgenesingroups = rep(nofg, 15), group = groups, expvals = as.data.frame(data)) 

	} else {
		
		ss=sample(c(nofgl:nofgu), 15, replace = TRUE)
#		ss[1]=sum(ss[2:15])*2
		sst=sum(ss)

		rep1=mat.or.vec(sst,6)
		rep2=mat.or.vec(sst,6)

		for(i in 1:ss[1]){
			rep1[i,1]=runif(1,2,4)
			rep1[i,2]=rep1[i,1]+runif(1,-0.2,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,-0.2,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,-0.2,0.2)
			rep1[i,5]=rep1[i,4]+runif(1,-0.2,0.2)
			rep1[i,6]=rep1[i,5]+runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,2,4)
			rep2[i,2]=rep2[i,1]+runif(1,-0.2,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,-0.2,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,-0.2,0.2)
			rep2[i,5]=rep2[i,4]+runif(1,-0.2,0.2)
			rep2[i,6]=rep2[i,5]+runif(1,-0.2,0.2)
		}

		s=ss[1]+1
		e=sum(ss[1:2])

		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0.2,0.6)
			rep1[i,4]=rep1[i,3]+runif(1,0.6,1.2)
			rep1[i,5]=rep1[i,4]+runif(1,1.2,2)
			rep1[i,6]=rep1[i,5]+runif(1,2,3)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,0,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,0.2,0.6)
			rep2[i,4]=rep2[i,3]+runif(1,0.6,1.2)
			rep2[i,5]=rep2[i,4]+runif(1,1.2,2)
			rep2[i,6]=rep2[i,5]+runif(1,2,3)
		}

		s=sum(ss[1:2])+1
		e=sum(ss[1:3])

		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,0.4,0.8)
			rep1[i,5]=rep1[i,4]+runif(1,0.4,0.8)
			rep1[i,6]=rep1[i,5]+runif(1,1,1.2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,0,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,0,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,0.4,0.8)
			rep2[i,5]=rep2[i,4]+runif(1,0.4,0.8)
			rep2[i,6]=rep2[i,5]+runif(1,1,1.2)
		}

		s=sum(ss[1:3])+1
		e=sum(ss[1:4])

		for(i in s:e){
			rep1[i,1]=runif(1,4,6)
			rep1[i,2]=rep1[i,1]+runif(1,-0.2,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,-0.2,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]+runif(1,2,4)
			rep1[i,6]=rep1[i,5]+runif(1,1,2)
	
			rep2[i,1]=runif(1,4,6)
			rep2[i,2]=rep2[i,1]+runif(1,-0.2,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,-0.2,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]+runif(1,2,4)
			rep2[i,6]=rep2[i,5]+runif(1,1,2)
		}

		s=sum(ss[1:4])+1
		e=sum(ss[1:5])
	
		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,8,10)
			rep1[i,3]=rep1[i,2]-runif(1,5,7)
			rep1[i,4]=rep1[i,3]-runif(1,1,2)
			rep1[i,5]=rep1[i,4]-runif(1,0.5,1)
			rep1[i,6]=rep1[i,5]-runif(1,1,2)
		
			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,8,10)
			rep2[i,3]=rep2[i,2]-runif(1,5,7)
			rep2[i,4]=rep2[i,3]-runif(1,1,2)
			rep2[i,5]=rep2[i,4]-runif(1,0.5,1)
			rep2[i,6]=rep2[i,5]-runif(1,1,2)
		}

		s=sum(ss[1:5])+1
		e=sum(ss[1:6])
	
		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,5,7)
			rep1[i,3]=rep1[i,2]+runif(1,2,4)
			rep1[i,4]=rep1[i,3]-runif(1,2,4)
			rep1[i,5]=rep1[i,4]-runif(1,1.2,2)
			rep1[i,6]=rep1[i,5]-runif(1,2,3)
	
			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,5,7)
			rep2[i,3]=rep2[i,2]+runif(1,2,4)
			rep2[i,4]=rep2[i,3]-runif(1,2,4)
			rep2[i,5]=rep2[i,4]-runif(1,1.2,2)
			rep2[i,6]=rep2[i,5]-runif(1,2,3)
		}

		s=sum(ss[1:6])+1
		e=sum(ss[1:7])
	
		for(i in s:e){
			rep1[i,1]=runif(1,2,4)
			rep1[i,2]=rep1[i,1]+runif(1,1,2)
			rep1[i,3]=rep1[i,2]+runif(1,1,2)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]-runif(1,2,4)
			rep1[i,6]=rep1[i,5]-runif(1,1,2)

			rep2[i,1]=runif(1,2,4)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,1,2)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]-runif(1,2,4)
			rep2[i,6]=rep2[i,5]-runif(1,1,2)
		}

		s=sum(ss[1:7])+1
		e=sum(ss[1:8])
	
		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,1,2)
			rep1[i,3]=rep1[i,2]+runif(1,2,3)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]+runif(1,2,3)
			rep1[i,6]=rep1[i,5]-runif(1,4,5)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,2,3)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]+runif(1,2,3)
			rep2[i,6]=rep2[i,5]-runif(1,4,5)
		}

		s=sum(ss[1:8])+1
		e=sum(ss[1:9])

		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,5,7)
			rep1[i,3]=rep1[i,2]+runif(1,2,4)
			rep1[i,4]=rep1[i,3]-runif(1,1,3)
			rep1[i,5]=rep1[i,4]+runif(1,-0.2,0.2)
			rep1[i,6]=rep1[i,5]+runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,5,7)
			rep2[i,3]=rep2[i,2]+runif(1,2,4)
			rep2[i,4]=rep2[i,3]-runif(1,1,3)
			rep2[i,5]=rep2[i,4]+runif(1,-0.2,0.2)
			rep2[i,6]=rep2[i,5]+runif(1,-0.2,0.2)
		}

		s=sum(ss[1:9])+1
		e=sum(ss[1:10])

		for(i in s:e){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]-runif(1,4,6)
			rep1[i,3]=rep1[i,2]-runif(1,2,2.5)
			rep1[i,4]=rep1[i,3]+runif(1,3,5)
			rep1[i,5]=rep1[i,4]+runif(1,1,3)
			rep1[i,6]=rep1[i,5]+runif(1,1,3)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]-runif(1,4,6)
			rep2[i,3]=rep2[i,2]-runif(1,2,2.5)
			rep2[i,4]=rep2[i,3]+runif(1,3,5)
			rep2[i,5]=rep2[i,4]+runif(1,1,3)
			rep2[i,6]=rep2[i,5]+runif(1,1,3)
		}

		s=sum(ss[1:10])+1
		e=sum(ss[1:11])

		for(i in s:e){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]+runif(1,3,5)
			rep1[i,3]=rep1[i,2]-runif(1,2,4)
			rep1[i,4]=rep1[i,3]-runif(1,3,5)
			rep1[i,5]=rep1[i,4]-runif(1,1,2)
			rep1[i,6]=rep1[i,5]+runif(1,2,3)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]+runif(1,3,5)
			rep2[i,3]=rep2[i,2]-runif(1,2,4)
			rep2[i,4]=rep2[i,3]-runif(1,3,5)
			rep2[i,5]=rep2[i,4]-runif(1,1,2)
			rep2[i,6]=rep2[i,5]+runif(1,2,3)
		}

		s=sum(ss[1:11])+1
		e=sum(ss[1:12])

		for(i in s:e){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]-runif(1,4,6)
			rep1[i,3]=rep1[i,2]+runif(1,4,6)
			rep1[i,4]=rep1[i,3]+runif(1,-0.2,0.2)
			rep1[i,5]=rep1[i,4]+runif(1,-0.2,0.2)
			rep1[i,6]=rep1[i,5]+runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]-runif(1,4,6)
			rep2[i,3]=rep2[i,2]+runif(1,4,6)
			rep2[i,4]=rep2[i,3]+runif(1,-0.2,0.2)
			rep2[i,5]=rep2[i,4]+runif(1,-0.2,0.2)
			rep2[i,6]=rep2[i,5]+runif(1,-0.2,0.2)
		}

		s=sum(ss[1:12])+1
		e=sum(ss[1:13])

		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0.2,0.6)
			rep1[i,4]=rep1[i,3]+runif(1,0.6,1.2)
			rep1[i,5]=rep1[i,4]+runif(1,1.2,2)
			rep1[i,6]=rep1[i,5]+runif(1,2,3)

			rep2[i,1]=runif(1,2,4)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,1,2)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]-runif(1,2,4)
			rep2[i,6]=rep2[i,5]-runif(1,1,2)
		}

		s=sum(ss[1:13])+1
		e=sum(ss[1:14])

		for(i in s:e){
			rep1[i,1]=runif(1,3,5)
			rep1[i,2]=rep1[i,1]+runif(1,1,2)
			rep1[i,3]=rep1[i,2]+runif(1,2,3)
			rep1[i,4]=rep1[i,3]+runif(1,1,2)
			rep1[i,5]=rep1[i,4]+runif(1,2,3)
			rep1[i,6]=rep1[i,5]-runif(1,-0.2,0.2)

			rep2[i,1]=runif(1,3,5)
			rep2[i,2]=rep2[i,1]+runif(1,1,2)
			rep2[i,3]=rep2[i,2]+runif(1,2,3)
			rep2[i,4]=rep2[i,3]+runif(1,1,2)
			rep2[i,5]=rep2[i,4]+runif(1,2,3)
			rep2[i,6]=rep2[i,5]-runif(1,-0.2,0.2)
		}

		s=sum(ss[1:14])+1
		e=sum(ss[1:15])

		for(i in s:e){
			rep1[i,1]=runif(1,7,9)
			rep1[i,2]=rep1[i,1]+runif(1,0,0.2)
			rep1[i,3]=rep1[i,2]+runif(1,0,0.2)
			rep1[i,4]=rep1[i,3]+runif(1,0.4,0.8)
			rep1[i,5]=rep1[i,4]+runif(1,0.4,0.8)
			rep1[i,6]=rep1[i,5]+runif(1,1,1.2)

			rep2[i,1]=runif(1,7,9)
			rep2[i,2]=rep2[i,1]+runif(1,0,0.2)
			rep2[i,3]=rep2[i,2]+runif(1,0,0.2)
			rep2[i,4]=rep2[i,3]+runif(1,0.4,0.8)
			rep2[i,5]=rep2[i,4]+runif(1,0.4,0.8)
			rep2[i,6]=rep2[i,5]+runif(1,1,1.2)
		}

		groups <- 0
		for(i in 1:15) {
			groups <- cbind(groups, t(as.matrix(rep(i, ss[i]))))
		}
		groups <- groups[-1]
		data <- cbind(rep1[, tpl:tpu], rep2[, tpl:tpu])
		dataframe <- list(nofgenesingroups = ss, group = groups, expvals = as.data.frame(data)) 
	}
	data <- dataframe
   return(data)
}	