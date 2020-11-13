# tutorial on Holland QFL

# load example txt 
sandstone = read.table('provenanceExample.txt',
                       header=TRUE, row.names=1, sep=',')


provenancePlot(sandstone, 'QFL', pch = 16)

provenancePlot(sandstone, 'QmFLt', pch = 16)

provenancePlot(sandstone, 'allFour', pch = 16)