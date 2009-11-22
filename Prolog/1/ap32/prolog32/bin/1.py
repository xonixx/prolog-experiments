import os
def changeEncoding(filePath, FROM, TO):
    '''take a full path to a file as input, and change its encoding from gb18030 to utf-16'''
    print filePath

    tempName=filePath+'~-~'

    input = open(filePath,'rb')
    content=unicode(input.read(),FROM)
    input.close()

    output = open(tempName,'wb')
    output.write(content.encode(TO))
    output.close()

    os.rename(tempName,filePath.replace(".pl", ".new.pl"))
    
changeEncoding("auto.pl", "latin1", "utf-8")

