import numpy as np

mydft, mygt = [], []
out = open('exp1_results.csv', 'w')
out.write('partID,trial,dft,class,gtsize,partsize,accuracy'+'\n')
with open('../raw_data/exp1_allparticipants_results.txt','r') as mydata:
	content = mydata.readlines()
	dft = content[-1]
	dft = dft.split('\t')
	for n, el in enumerate(dft):
		if n==0:
			pass
		else:
			mydft.append(float(el))
	print(mydft)
	gt = content[-2]
	gt = gt.split('\t')
	for n, el in enumerate(gt):
		if n==0:
			pass
		else:
			el = el.strip()
			mygt.append(str(el))
	print(mygt)

	for n,line in enumerate(content):
		if n==0:
			pass
		elif line.startswith('DFT'):
			pass
		elif line.startswith('GT'):
			pass
		else:
			fields = line.strip().split('\t')
			partID = fields[0]
			for nn, resp in enumerate(fields[1:]):
				gtresp = str(mygt[nn])
				dftresp = float(mydft[nn])
				if float(dftresp) < 0.20:
					myclass = 'borderline'
				else:
					myclass = 'clearcut'
				if str(resp) == gtresp:
					myresp = 1
				else:
					myresp = 0
				print(partID,nn+1,dftresp,gtresp,resp,myresp)
				out.write(str(partID)+','+str(nn+1)+','+str(dftresp)+','+str(myclass)+','+str(gtresp)+','+str(resp)+','+str(myresp)+'\n')

