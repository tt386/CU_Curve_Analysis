import sys
import numpy as np
import os


def gradient(u, x):
    return np.gradient(u, x, edge_order=2)

def periodic_slice(arr, center_index, width):
    """Return a periodic slice of `arr` centered on `center_index` with total length `width`."""
    n = len(arr)
    half = width // 2
    indices = (np.arange(center_index - half, center_index + half) % n)
    return arr[indices]



def main():
    if len(sys.argv) != 2:
        print("Usage: python process_npz_to_dat.py <file.npz>")
        sys.exit(1)

    npz_file = sys.argv[1]

    try:
        data = np.load(npz_file)
        x = data['x']
        u = data['u']
        w = data['v']

        if x.shape != u.shape or x.shape != w.shape:
            raise ValueError("x, u, and w must have the same shape.")


        v = gradient(u,x)

        center = np.argmax(u)
        u = periodic_slice(u, center, 1024)
        v = periodic_slice(v, center, 1024)
        w = periodic_slice(w, center, 1024)
        x = periodic_slice(x, center, 1024)



        # Normalize x to range [0, 1]
        x_min, x_max = np.min(x), np.max(x)
        x_norm = (x - x_min) / (x_max - x_min)

        #v = gradient(u,x)#periodic_gradient(u, x)


        # Reverse u, v, w
        u = u[::-1]
        v = v[::-1]
        w = w[::-1]


        output_filename = "Outdata_" + os.path.splitext(os.path.basename(npz_file))[0] + ".dat"

        output_data = np.column_stack((x_norm, u, v, w))
        np.savetxt(output_filename, output_data, fmt='%.8e', delimiter=' ')

        print(f"Saved output to {output_filename}")

    except KeyError as e:
        print(f"Missing expected key in .npz file: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error processing file: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()

