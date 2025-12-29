import numpy as np

# Using readlines()
file = open('euler18.txt', 'r')
data_arr = [np.array([x for x in line.rstrip().split(' ')], dtype=int) for line in file.readlines()]
N = len(data_arr)
print(data_arr)

M = np.zeros((N,N), dtype=int)
for i in range(N):
    # Diagonal
    M[i,i] = data_arr[i][i]
    # Symmetric entries
    for j in range(i):
        M[i,j] = data_arr[i][j]

print(M)