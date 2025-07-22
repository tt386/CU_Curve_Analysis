import sys
import numpy as np
import matplotlib.pyplot as plt

def main():
    if len(sys.argv) != 2:
        print("Usage: python plot_data.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]

    try:
        # Load data assuming space-delimited, with 4 columns
        data = np.loadtxt(filename)

        if data.shape[1] < 4:
            raise ValueError("Expected at least 4 columns in the input file.")

        x = data[:, 0]
        u = data[:, 1]
        v = data[:, 2]
        w = data[:, 3]

        plt.plot(x, u, label='u(x)')
        plt.plot(x, v, label='v(x)')
        plt.plot(x, w, label='w(x)')

        plt.xlabel('x')
        plt.ylabel('Function values')
        plt.title('u(x), v(x), and w(x)')
        plt.legend()
        plt.grid(True)
        plt.tight_layout()
        plt.show()

    except Exception as e:
        print(f"Error reading file '{filename}': {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
