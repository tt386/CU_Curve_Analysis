import matplotlib.pyplot as plt
import numpy as np
def ExtractFromDat(filename):
    # Initialize the list to store all blocks of data
    all_data = []
    current_block = []

    # Open the .dat file
    with open(filename, 'r') as file:
        for line in file:
            # Strip leading/trailing whitespace
            line = line.strip()
            
            # If the line is empty, we have reached the end of the current block of data
            if not line:
                if current_block:  # Only add non-empty blocks
                    all_data.append(current_block)
                    current_block = []  # Reset for the next block
            else:
                # Split the line into individual values
                values = line.split()
                
                # Ensure there are exactly 6 values in the line
                if len(values) == 6:
                    # Convert values to float and add to the current block
                    current_block.append([float(val) for val in values])
                else:
                    print(f"Skipping line due to unexpected number of values: {line}")

        # If there's any remaining block after the last non-empty line, append it
        if current_block:
            all_data.append(current_block)

    return all_data


# Example of usage:
plt.figure()

#DATA
filename = 'hom.dat'  # Replace with your actual filename
blocks = ExtractFromDat(filename)
for i in range(len(blocks)):
    block = np.asarray(blocks[i])
    plt.plot(block[:,0],block[:,5],color='k',label='_nolegend_')


filename = 'u_curve.dat'  # Replace with your actual filename
blocks = ExtractFromDat(filename)

for i in range(len(blocks)):
    block = np.asarray(blocks[i])
    plt.plot(block[:,0],block[:,5],color='r',label='_nolegend_')

plt.grid()

plt.ylabel("c")
plt.xlabel("p")


#Add legends manually
handles = [
    plt.Line2D([0],[0],color='k',label='C'),
    plt.Line2D([0],[0],color='r',label='U'),
        ]

plt.legend(handles=handles,loc='upper left')

plt.ylim(0,10)
plt.xlim(-2.5,0.5)

plt.title("Karma Ramp")

plt.savefig("CU.png")









from scipy import optimize

us = 1.5
M=4
d=0.25
e=0.01
nb=0.5
D=1
k_small=0.01




def H(x,k):
    return x/(1 + np.exp(-2*x/k))

stable_I = []
stable_c = []

unstable_I =[]
unstable_c = []


def V_Nullcline(u):
    return H(u-1,k_small)/nb

def W_Nullcline(u):
    
    #return np.where(us + (I-u)/(2*(u**2-d*u**3)) < 0,-1,(us + (I-u)/(2*(u**2-d*u**3)))**(1/M))
    """
    if us + (I-u)/(2*(u**2-d*u**3)) > 0:
        return (us + (I-u)/(2*(u**2-d*u**3)))**(1/M)
    else:
        return -1 
    """
    return (us + (I-u)/(2*(u**2-d*u**3)))**(1/M)

def DiffFunc(V):
    return V_Nullcline(V) - W_Nullcline(V)



u = np.linspace(-1,2,100)

for i in range(len(blocks)):
    block = np.asarray(blocks[i])
    for i in range(len(block[:,0])):
        I = block[:,0][i]
        c_orig = block[:,5][i]
        print(I,c_orig)
        for offset in [-0.1,0.1]:
            c = c_orig + offset

            root = optimize.newton(DiffFunc,1.5)
    
            u = root
            v = V_Nullcline(u)
            
            if u > 1:
                Jacobian = [[0,1,0],
                            [1/D * (-2 *(2*u-3*d*u**2)*(us-v**M) + 1),c/D,1/D*2*M*v**(M-1)*(u**2-d*u**3)],
                            [e/c / nb,0,-e/c]]
            else:
                Jacobian = [[0,1,0],
                            [1/D * (-2 *(2*u-3*d*u**2)*(us-v**M) + 1),c/D,1/D*2*M*v**(M-1)*(u**2-d*u**3)],
                            [0,0,-e/c]]


            eigenvalues,eigenvectors = np.linalg.eig(Jacobian)


            unstable = False
            for eigenval in eigenvalues:
                if np.imag(eigenval) != 0:
                    if np.real(eigenval) > 0:
                        unstable_I.append(I)
                        unstable_c.append(c)
                        unstable = True

                        break
            if not unstable:
                stable_I.append(I)
                stable_c.append(c)

plt.scatter(stable_I,stable_c,color='g',label='stable',s=0.9)
plt.scatter(unstable_I,unstable_c,color='b',label='unstable',s=0.9)
plt.legend()
plt.ylim(-0.1,10)
plt.savefig("Plotting_Stability.png")









"""
# Example: Print the first block of data
print(blocks[0])  # Prints the first block (a list of lists)
"""
