import tkinter as tk
from tkinter import filedialog, messagebox, ttk

from ni_agent2.services.excel_analyzer import analyze_excel
from ni_agent2.services.outlook_analyzer import analyze_eml_file, analyze_recent_outlook_emails, outlook_available
from ni_agent2.services.ppt_generator import generate_presentation


class MainWindow(ttk.Frame):
    def __init__(self, master: tk.Tk) -> None:
        super().__init__(master)
        self.master = master
        self.pack(fill=tk.BOTH, expand=True)
        self._build_ui()

    def _build_ui(self) -> None:
        notebook = ttk.Notebook(self)
        notebook.pack(fill=tk.BOTH, expand=True)

        notebook.add(self._build_ppt_tab(notebook), text="PPT 生成")
        notebook.add(self._build_excel_tab(notebook), text="Excel 分析")
        notebook.add(self._build_outlook_tab(notebook), text="Outlook 分析")

    def _build_ppt_tab(self, parent: ttk.Notebook) -> ttk.Frame:
        frame = ttk.Frame(parent)
        frame.columnconfigure(1, weight=1)
        frame.rowconfigure(3, weight=1)

        ttk.Label(frame, text="Deck 标题").grid(row=0, column=0, sticky=tk.W, padx=8, pady=6)
        self.ppt_title = ttk.Entry(frame)
        self.ppt_title.grid(row=0, column=1, sticky=tk.EW, padx=8, pady=6)

        ttk.Label(frame, text="保存路径").grid(row=1, column=0, sticky=tk.W, padx=8, pady=6)
        self.ppt_path = ttk.Entry(frame)
        self.ppt_path.grid(row=1, column=1, sticky=tk.EW, padx=8, pady=6)
        ttk.Button(frame, text="选择", command=self._select_ppt_path).grid(row=1, column=2, padx=8, pady=6)

        ttk.Label(frame, text="大纲 (每行一页, 用 | 分隔标题和要点, 要点用 ; 分隔)").grid(
            row=2, column=0, columnspan=3, sticky=tk.W, padx=8, pady=6
        )
        self.ppt_outline = tk.Text(frame, height=10)
        self.ppt_outline.grid(row=3, column=0, columnspan=3, sticky=tk.NSEW, padx=8, pady=6)

        ttk.Button(frame, text="生成 PPT", command=self._generate_ppt).grid(
            row=4, column=0, columnspan=3, pady=10
        )

        return frame

    def _build_excel_tab(self, parent: ttk.Notebook) -> ttk.Frame:
        frame = ttk.Frame(parent)
        frame.columnconfigure(1, weight=1)
        frame.rowconfigure(2, weight=1)

        ttk.Label(frame, text="Excel/CSV 文件").grid(row=0, column=0, sticky=tk.W, padx=8, pady=6)
        self.excel_path = ttk.Entry(frame)
        self.excel_path.grid(row=0, column=1, sticky=tk.EW, padx=8, pady=6)
        ttk.Button(frame, text="选择", command=self._select_excel).grid(row=0, column=2, padx=8, pady=6)

        ttk.Button(frame, text="分析", command=self._analyze_excel).grid(row=1, column=0, columnspan=3)

        self.excel_output = tk.Text(frame, height=12)
        self.excel_output.grid(row=2, column=0, columnspan=3, sticky=tk.NSEW, padx=8, pady=6)

        return frame

    def _build_outlook_tab(self, parent: ttk.Notebook) -> ttk.Frame:
        frame = ttk.Frame(parent)
        frame.columnconfigure(1, weight=1)
        frame.rowconfigure(3, weight=1)

        ttk.Label(frame, text="Outlook (Windows) 最近邮件数量").grid(row=0, column=0, sticky=tk.W, padx=8, pady=6)
        self.outlook_limit = ttk.Entry(frame)
        self.outlook_limit.insert(0, "25")
        self.outlook_limit.grid(row=0, column=1, sticky=tk.EW, padx=8, pady=6)
        ttk.Button(frame, text="分析 Inbox", command=self._analyze_outlook).grid(row=0, column=2, padx=8, pady=6)

        ttk.Separator(frame, orient=tk.HORIZONTAL).grid(row=1, column=0, columnspan=3, sticky=tk.EW, pady=6)

        ttk.Label(frame, text="或选择 .eml 文件").grid(row=2, column=0, sticky=tk.W, padx=8, pady=6)
        self.eml_path = ttk.Entry(frame)
        self.eml_path.grid(row=2, column=1, sticky=tk.EW, padx=8, pady=6)
        ttk.Button(frame, text="选择", command=self._select_eml).grid(row=2, column=2, padx=8, pady=6)

        ttk.Button(frame, text="分析 EML", command=self._analyze_eml).grid(row=3, column=0, columnspan=3)

        self.outlook_output = tk.Text(frame, height=12)
        self.outlook_output.grid(row=4, column=0, columnspan=3, sticky=tk.NSEW, padx=8, pady=6)

        if not outlook_available():
            self.outlook_output.insert(tk.END, "提示: 当前环境无法访问 Outlook MAPI, 请使用 .eml 文件。\n")

        return frame

    def _select_ppt_path(self) -> None:
        path = filedialog.asksaveasfilename(defaultextension=".pptx", filetypes=[("PowerPoint", "*.pptx")])
        if path:
            self.ppt_path.delete(0, tk.END)
            self.ppt_path.insert(0, path)

    def _select_excel(self) -> None:
        path = filedialog.askopenfilename(filetypes=[("Excel/CSV", "*.xlsx *.xls *.csv")])
        if path:
            self.excel_path.delete(0, tk.END)
            self.excel_path.insert(0, path)

    def _select_eml(self) -> None:
        path = filedialog.askopenfilename(filetypes=[("EML", "*.eml")])
        if path:
            self.eml_path.delete(0, tk.END)
            self.eml_path.insert(0, path)

    def _generate_ppt(self) -> None:
        title = self.ppt_title.get().strip()
        outline = self.ppt_outline.get("1.0", tk.END).strip()
        output = self.ppt_path.get().strip()

        if not output:
            messagebox.showerror("错误", "请先选择保存路径")
            return

        slide_count = generate_presentation(title, outline, output)
        messagebox.showinfo("完成", f"PPT 已生成: {output}\n共 {slide_count} 页")

    def _analyze_excel(self) -> None:
        path = self.excel_path.get().strip()
        if not path:
            messagebox.showerror("错误", "请先选择 Excel/CSV 文件")
            return

        try:
            output = analyze_excel(path)
        except Exception as exc:
            messagebox.showerror("分析失败", str(exc))
            return

        self.excel_output.delete("1.0", tk.END)
        self.excel_output.insert(tk.END, output)

    def _analyze_outlook(self) -> None:
        if not outlook_available():
            messagebox.showerror("不可用", "当前环境无法访问 Outlook MAPI, 请使用 .eml 文件。")
            return

        try:
            limit = int(self.outlook_limit.get().strip())
        except ValueError:
            messagebox.showerror("错误", "请输入数字")
            return

        try:
            output = analyze_recent_outlook_emails(limit)
        except Exception as exc:
            messagebox.showerror("分析失败", str(exc))
            return

        self.outlook_output.delete("1.0", tk.END)
        self.outlook_output.insert(tk.END, output)

    def _analyze_eml(self) -> None:
        path = self.eml_path.get().strip()
        if not path:
            messagebox.showerror("错误", "请先选择 .eml 文件")
            return

        try:
            output = analyze_eml_file(path)
        except Exception as exc:
            messagebox.showerror("分析失败", str(exc))
            return

        self.outlook_output.delete("1.0", tk.END)
        self.outlook_output.insert(tk.END, output)
