import pickle


def load_pickle(file_path):
    with open(file_path, "rb") as f:
        data = pickle.load(f)
    return data


def print_pickle_contents(file_path):
    data = load_pickle(file_path)
    print(data)


# replace 'file_path.pkl' with your actual file path
print_pickle_contents(r"C:\Users\srude\Desktop\stores.pkl")
