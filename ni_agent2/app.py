import tkinter as tk

from ni_agent2.ui.main_window import MainWindow


def main() -> None:
    root = tk.Tk()
    root.title("Ni Agent2")
    root.geometry("900x600")
    MainWindow(root)
    root.mainloop()


if __name__ == "__main__":
    main()
