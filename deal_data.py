# -*- coding: utf-8 -*-
"""
Created on Sat Jun  6 09:38:18 2020

@author: Administrator
"""

##########################预处理
#计算fa的K-mer子串(k=6),用K-mer数组成特征向量
def readfa(dr):
    f = open(dr,'r')
    lines = f.readlines()
    result = {}
    k = 6
    #统计一条序列总共有多少条K-mer
    static_kmer = {}
    for i in lines :
        ##用于一个K-mer的计数
        count = 0
        if '>' in i :
            j = i.strip().split('>')[1]
            result[j] = {}
        else:
            i = i.strip()
            n = len (i)
            #######################
            #一条序列的k-mer总数数=序列长度-k+1=len(i)-5
            ln = len(i.strip())-5
             #统计一条序列总共有多少条K-mer
            static_kmer[j] = ln 
            #################
            for l in range(n-5):
                m = i[l:l+k]
                if m not in result[j].keys():
                    result[j][m] = 1
                else :
                    result[j][m] += 1
                    
            
            
            
    f.close()
    return(result,static_kmer)

##调用readfa函数，ABI5_neg.txt;ABI5_pos.txt
f,static_kmer = readfa('ABI5_pos.txt')####修改ABI5_neg.txt


###用K-mer(196)频率组成特征向量
# f.keys()返回第几条序列，eg: neg 1
t = f.keys()
#一条序列的一种k-mer频率 = f[i][k-mer] / static_kmer[i]
static_pl = {}
for i in t :
#####f['neg 1']['AGAAAA'] = 1
    l = f[i].keys()
    static_pl[i]= {}
    for j in l :
#####k-mer频率计算
        pl = float(f[i][j]/static_kmer[i])
        static_pl[i][j] = pl

####将得到的k-mer频率的字典整成 n * 196维的数组，并保存到相应文件中(最后一列表示：正样本标为:1，负样本标为：0)
f_kmer = open('ABI5_pos_kmer.txt','w') ###修改
 
t_pl = static_pl.keys() 
for k in t_pl:
    l_pl = static_pl[k].keys()
    for n in l_pl :
        #保留4位小数
        f_kmer.write('%.4f'%(static_pl[k][n])+'\t')
  ###补0
    if len(static_pl[k]) == 196 :
        f_kmer.write('1'+'\n') ###修改（pos）1或(neg)0
    else :
        for i in range(196-len(static_pl[k])) :
            f_kmer.write('0'+'\t')
        f_kmer.write('1'+'\n') ##修改（pos）1或(neg)0
f_kmer.close()

####训练集：测试集=7：3，cut函数或者linux里分好，合并好
##




    