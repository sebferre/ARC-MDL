import sys
import os
import json
from PIL import Image

# options

cell_width = 19
border_width = 1
border_color = (128,128,128) # grey
bg_color = (255,255,255) # white
grid_span = 15
train_test_sep_color = (255,0,0) # red

# reading task files and folders

def load_task(file_path):
    try:
        with open(file_path, 'r') as file:
            data = json.load(file)
        return data
    except FileNotFoundError:
        print("Error: File not found")
        return None
    except json.JSONDecodeError:
        print("Error: Invalid JSON format")
        return None

def get_json_files(folder_path):
    json_files = []
    for file_name in os.listdir(folder_path):
        if file_name.endswith('.json'):
            json_files.append(os.path.join(folder_path, file_name))
    return json_files

# generating images
    
colors = [(0,0,0), # black
          (65,105,225), # blue
          (255,0,0), # red
          (50,205,50), # green
          (255,255,0), # yellow
          (211,211,211), # grey
          (255,0,255), # pink
          (255,165,0), # orange
          (0,255,255), # cyan
          (165,42,42), # brown
          (255,255,255) # white (transparent)
          ]

color_cells = [Image.new('RGB', (cell_width,cell_width), color)
               for color in colors]

def offset(i):
    return i * (cell_width + border_width) + border_width

def grid_image(grid):
    k = len(grid)
    l = len(grid[0])
    h = offset(k)
    w = offset(l)
    img = Image.new('RGB', (w,h), border_color)
    for i in range(0,k):
        for j in range(0,l):
            c = grid[i][j]
            if c >= 0 and c <= 10:
                img.paste(color_cells[c], (offset(j), offset(i)))
    return img

def task_image(task):
    train = task["train"]
    test = task["test"]
    n_train = len(train)
    n_test = len(test)
    n = n_train + n_test
    grid_pairs = [(grid_image(pair["input"]), grid_image(pair["output"]))
                  for pair in (train + test)]
    maxhi = 0
    maxho = 0
    maxw = 0
    for (gi,go) in grid_pairs:
        wi, hi = gi.size
        wo, ho = go.size
        maxhi = max(maxhi,hi)
        maxho = max(maxho,ho)
        maxw = max(maxw,wi,wo)
    assert(maxhi > 0)
    assert(maxho > 0)
    assert(maxw > 0)
    hi = maxhi + 2 * grid_span
    ho = maxho + 2 * grid_span
    w = maxw + 2 * grid_span
    img = Image.new('RGB', (w * n, hi + ho), bg_color)
    train_test_sep = Image.new('RGB', (2, hi + ho), train_test_sep_color)
    img.paste(train_test_sep, (w * n_train - 1, 0))
    for (i, (gi,go)) in enumerate(grid_pairs):
        img.paste(gi, (i * w + grid_span, grid_span))
        img.paste(go, (i * w + grid_span, hi + grid_span))
    return img, grid_pairs

# main functions

def process_file(json_file, img_folder, flag_grids):
    task = load_task(json_file)
    if task:
        img, grid_pairs = task_image(task)
        basename, _ = os.path.splitext(json_file)
        _, filename = os.path.split(basename)
        print(filename)
        img_file = os.path.join(img_folder, filename + ".png")
        img.save(img_file)
        if flag_grids:
            for (i, (gi,go)) in enumerate(grid_pairs):
                gi.save(os.path.join(img_folder, filename + "_input" + str(i+1) + ".png"))
                go.save(os.path.join(img_folder, filename + "_output" + str(i+1) + ".png"))
    else:
        print("this JSON file could not be read: ", json_file)

def main():
    if len(sys.argv) < 2:
        print("Usage: python task2img.py <src_folder[/task.json]> <dest_folder> [grids]")
        sys.exit(1)
    src_path = sys.argv[1]
    img_folder = sys.argv[2]
    flag_grids = len(sys.argv) > 3 and sys.argv[3] == "grids"
    if src_path.endswith(".json"):
        process_file(src_path, img_folder, flag_grids)
    else:
        for filename in os.listdir(src_path):
            if filename.endswith('.json'):
                json_path = os.path.join(src_path, filename)
                process_file(json_path, img_folder, flag_grids)


if __name__ == "__main__":
    main()
