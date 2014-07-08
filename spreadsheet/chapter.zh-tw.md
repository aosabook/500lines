# 用[99 行程式](https://github.com/audreyt/500lines/tree/master/spreadsheet)寫出[試算表網站](http://audreyt.github.io/500lines/spreadsheet/) 

本章介紹以 99 行 HTML、CSS 和 JS 程式（瀏覽器原生支援的三種語言）寫成的試算表應用網站。

## 簡介

1990 年，Tim Berners-Lee 發明了全球資訊網，當時的網頁（Web pages）都是以 HTML 寫成，使用尖括號的標籤（tags）來標記文字，給內容安排邏輯結構。以 `<a>…</a>` 標記的文字會變成超連結（hyperlinks），把使用者導引至其他網頁。

在 20 世紀 90 年代，瀏覽器加入了各種展示性標籤到 HTML 詞彙，包括一些聲名狼藉的非標準標籤，例如來自 Netscape Navigator 的 `<blink>…</blink>` 和來自 Internet Explorer 的 `<marquee>…</marquee>` ，都造成了親和力和瀏覽器兼容性的廣泛問題。

為了保持 HTML 的原來目的 —— 描述一個文件的邏輯結構，瀏覽器開發者最終同意支援額外兩種程式語言：CSS 來形容網頁的展示風格，以及 JS 來描述其動態互動功能。

從那時開始，這三種程式語言經過了 20 年的共同進化，已經變得更加簡潔和強大。如今，跨平台的應用網站（Web applications，例如試算表），已經跟上個世紀的桌面應用程式（如 VisiCalc、Lotus 1-2-3 和 Excel）一樣普及了。

應用網站可以在 99 行裡面提供多少功能？讓我們來看看！

## 概述

在 [spreadsheet](https://github.com/audreyt/500lines/tree/master/spreadsheet) 目錄裡，包含了三種 Web 程式語言在 2014 年末版本的展示範例：描述結構的 [HTML5](http://www.w3.org/TR/html5/)、描述展示風格的 [CSS3](http://www.w3.org/TR/css3-ui/)，以及描述互動功能的 JS [ES6 “Harmony”](http://wiki.ecmascript.org/doku.php?id=harmony:specification_drafts) 。它也用到 [Web Storage](http://www.whatwg.org/specs/web-apps/current-work/multipage/webstorage.html) 來保存資料，以及利用 [Web Worker](http://www.whatwg.org/specs/web-apps/current-work/multipage/workers.html) 在背景運行 JS 程式碼。在撰寫本文時，這些 Web 標準都已獲得 Firefox、Chrome、Internet Explorer 11+，以及手機行動瀏覽器 iOS 5+ 和 Android 4+ 的支援。


現在讓我們在瀏覽器中打開 <http://audreyt.github.io/500lines/spreadsheet/>：

![初始畫面](./images/01-initial.png)

### 基本概念

試算表往兩個方向延伸，直欄（columns）由 **A** 開始編號，而橫列（rows）則由 **1** 開始。每個儲存格（cell）都有各自的座標（coordinate）與內容（content），例如 **A1** 與 `1874`。內容的型別（types）有以下四種：

* 文字：**B1** 中的 `+` 和 **D1** 中的 `⇒`，向左對齊。
* 數字：**A1** 中的 `1874` 和 **C1** 中的 `2046`， 向右對齊。
* 公式：**E1** 中的 `=A1+C1`，經計算（calculation）後得出數值（value）為 `3920`，以淡藍色背景顯示。
* 空白：橫列 **2** 上的所有儲存格目前都是空白的。

點擊 `3920` 來移動焦點（focus）到 **E1**，會出現輸入框（input box）來顯示它的公式：

![輸入框](./images/02-input.png)

現在把選取焦點移到 **A1** ，將它的內容變更（change）為 `1`，使 **E1** 的數值重新計算（recalculate）成 `2047`：

![已變更的內容](./images/03-changed.png)

按 **輸入鍵** 以設定焦點至 **A2**，並變更其內容為 `=Date()`，然後按 **跳格鍵**，變更 **B2** 的內容為 `=alert()`，然後再按 **跳格鍵** 以移動焦點至 `C2`：

![公式錯誤](./images/04-error.png)

以上展示了公式的計算結果可以是數字（**E1** 中的 `2047`）、文字（**A2** 顯示的目前時間，向左對齊），或者是錯誤（error，如 **B2** 中的紅色字母，置中對齊）。

接下來試試輸入 `=for(;;){}`，這是一個 JS 程式碼，輸入後會執行一個永無休止的無限循環。試算表將會在嘗試變更時自動回復（restore） **C2** 的內容，以避免執行此循環。

現在按 **Ctrl-R** 或 **Cmd-R** 重新載入瀏覽器頁面，確認試算表的內容能夠延續（persistent），在每個瀏覽階段中保持一致。 如果要重置（reset）試算表的原始內容，可按左上角的 `↻` 按鈕。

### 漸進增強

在我們開始詳細討論 99 行程式碼之前，不妨先停用瀏覽器中的 JS ，並重新載入頁面，注意一下前後區別：

* 大型表格消失了，螢幕上只剩下一個 2x2 的表格以及單個內容儲存格。
* 列和欄的標示變成 `{{ row }}` 和 `{{ col }}`。
* 按下 `↻` 按鈕沒有任何反應。
* 按 **跳格鍵** 或點擊第一列的內容時，仍然會顯示可編輯的輸入框。

![停用 JavaScript](./images/05-nojs.png)

當我們停用動態互動功能（JS）時，內容結構（HTML）和展示風格（CSS）依然持續生效。如果網站能夠在 JS 和 CSS 同時停用的情況下，繼續保持可用，也就是遵守「漸進增強」（progressive enhancement）原則，就能讓它的內容接觸到最廣的使用者群。

由於我們的試算表是應用網站，並沒有伺服器端的程式碼，所以我們必須依賴 JS 來提供所需要的邏輯。然而，即使 CSS 沒有完全獲得支援， 比如說在螢幕閱讀器和文字模式的瀏覽器裡，它依然能夠正常運作：

![停用 CSS](./images/06-nocss.png)

如同以上畫面顯示，如果我們在瀏覽器中啟用 JS ，但轉而停用 CSS 的話，效果如下：

* 所有背景和前景顏色會消失。
* 輸入框和儲存格的數值會同時顯示，而不只是顯示其中一個。
* 除此之外，應用程式跟完全版一樣正常運作。

## 程式碼逐步解說

以下的架構圖描繪 HTML 和 JS 元件之間的各式連結：

![架構圖](./images/00-architecture.png)

為了理解這張圖，讓我們按照瀏覽器載入的次序，逐一檢視四份原始程式碼檔案：

* **index.html**: 20 行
* **main.js**: 36 行（不包括註釋和空白行）
* **worker.js**: 32 行（不包括註釋和空白行）
* **styles.css**: 11 行

### HTML

`index.html` 中的首行程式碼聲明這是使用 HTML5（`<!DOCTYPE html>`） 和 UTF-8 編碼寫成的：

```html
<!DOCTYPE html><html><head><meta charset="UTF-8">
```

如果沒有聲明 `charset` ，瀏覽器可能會把重置按鈕的萬國碼符號 `↻` 顯示為 `â†»`，也就是亂碼（mojibake）——由解碼問題引起的錯誤情況。

接下來的四行程式碼是 JS 聲明，依慣例放在 `head` 區塊裡：

```html
  <script src="main.js"></script>
  <script>if (!self.Spreadsheet) { location.href = "es5/index.html" }</script>
  <script src="worker.js"></script>
  <script src="lib/angular.js"></script>
```

標籤 `<script src="…">` 從跟 HTML 頁面相同的路徑載入 JS 資源。舉例來說，如果目前的網址是 `http://audreyt.github.io/500lines/spreadsheet/index.html`，那麼 `lib/angular.js` 會指向 `http://audreyt.github.io/500lines/spreadsheet/lib/angular.js`。

`if (!self.Spreadsheet)` 這行程式碼用於測試 `main.js` 是否已經正確載入；如果沒有正確載入，它會將瀏覽器轉至 `es5/index.html`。對於 2015 年以前發行、並且不支援 ES6 的瀏覽器來說，這個「優雅降級」（graceful degradation）的重新導向技巧，確保我們可以用轉譯到 ES5 版本的 JS 程式，來作為備用方案。

接下來的兩行程式碼會載入 CSS 資源，結束 `head` 部分，並開始 `body` 部分，當中包含使用者會看到的內容：

```html
  <link href="styles.css" rel="stylesheet">
</head><body ng-app ng-cloak ng-controller="Spreadsheet">
```

上述的 `ng-` 屬性指示 AngularJS 程式庫運行`Spreadsheet` 的 JS 函數，為此文件建立控制器（controller）來提供模型（model），也就是一組可以在文件顯示層（view）中進行繫結（binding）的名稱。`ng-cloak` 屬性會先隱藏文件顯示，直到繫結已經就位為止。

舉個具體的例子，當使用者點擊下一行中所定義的 `<button>` ，其 `ng-click` 屬性會觸發並執行 `reset()` 和 `calc()` 這兩個由 JS 模型提供的函式：

```html
  <table><tr>
    <th><button type="button" ng-click="reset(); calc()">↻</button></th>
```

下一行使用 `ng-repeat` ，在置頂列顯示各個直欄的標籤：

```html
    <th ng-repeat="col in Cols">{{ col }}</th>
```

舉例來說，如果 JS 模型把 `Cols` 定義為 `["A","B","C"]`，那麼將會有三個標題儲存格（`th`）來顯示標筏。內容的 `{{ col }}` 表達式會由 AngularJS 進行安插（interpolation），來在每個 `th` 的內容中填上當前的 `col` 數值。

同樣地，下面兩行會檢查 `Rows` 的數值（`[1,2,3]` 等等），為每個數值建立橫列，並在最左邊的 `th` 儲存格以編號標註：

```html
  </tr><tr ng-repeat="row in Rows">
    <th>{{ row }}</th>
```

由於使用 `<tr ng-repeat>` 標籤開始的程式碼還沒有被 `</tr>` 結束，表達式可以繼續使用  `row` 變數。下一行程式碼，在當前的橫列上建立一個資料儲存格（`td`），並在其 `ng-class` 屬性中同時使用 `col` 和 `row` 變數：

```html
    <td ng-repeat="col in Cols" ng-class="{ formula: ('=' === sheet[col+row][0]) }">
```

這裡有幾個重點。在 HTML 中，`class` 屬性描述類別名稱的集合（a set of class names），讓 CSS 以不同方式賦予它們模式。這裡的 `ng-class` 會運算表達式 `('=' === sheet[col+row][0])`；如果結果為真，那麼 `<td>`會獲得類別 `formula` ，因此儲存格就會添上淡藍色背景，由 **styles.css** 第 8 行中的 `.formula`「類別選取器」（class selector）所定義。

上述表達式用來檢查目前儲存格是否為公式的方法，是透過測試 `=` 是否為 `sheet[col+row]` 字串的初始字符（`[0]`）。此處 `sheet` 是 JS 模型物件，以各個座標（例如 `"E1"`） 為它的屬性，儲存格內容（例如 `"=A1+C1"`）則是屬性的值。要注意的是，由於 `col` 是字串而非數字，因此 `col+row` 中的 `+` 指的是串聯，而不是加法。

在 `<td>` 中，我們給使用者一個輸入框，來編輯儲存在 `sheet[col+row]` 中的儲存格內容：

```html
       <input id="{{ col+row }}" ng-model="sheet[col+row]" ng-change="calc()"
        ng-model-options="{ debounce: 200 }" ng-keydown="keydown( $event, col, row )">
```
這裡的主要屬性是 `ng-model`，它允許 JS 模型和輸入框內可編輯內容之間的「雙向繫結」（two-way binding）：每當使用者在輸入框內作出變更，JS 模型都會自動更新 `sheet[col+row]` 的內容，並觸發 `calc()` 函數來重新計算所有公式儲存格的數值。

當使用者按住某個鍵不放的時候，為了避免重複執行 `calc()`，`ng-model-options` 會限制更新速率至每 200 毫秒一次。

輸入框的 `id` 屬性，由 `col+row` 安插取得座標。 一個 HTML 元件的 `id` 屬性，必須跟文件內所有其他元件的 `id` 不同。這樣確保了 `#A1` 這個「ID 選取器」（ID selector）只會指向至一個元素，而不像類別選取器 `.formula` 那樣指稱一系列的元素。當使用者按下 **上鍵**/**下鍵**/**輸入鍵** 時，`keydown()` 中的邏輯就會使用 ID 選取器，來確定該設定焦點在哪個輸入框上。

在輸入框後面，我們放置一個 `<div>` 元素，來顯示當前儲存格的計算結果（以物件 `errs` 和 `vals` 在 JS 模型中表示）：

```html
      <div ng-class="{ error: errs[col+row], text: vals[col+row][0] }">
        {{ errs[col+row] || vals[col+row] }}</div>
```

如果計算公式發生錯誤，文字插值會使用 `errs[col+row]` 裡存放的錯誤訊息，而 `ng-class` 會套用 `error` 類別至該元件，為它套用特定的 CSS 樣式（使用紅色文字、置中對齊等等）。

如果沒有錯誤，進行安插的會是 `||` 右側的 `vals[col+row]`。如果它是一個非空白的字符串，初始字符（`[0]`）就會運算為真，使得 `text` 類別套用到元素上，使文字向左對齊。

由於空白字符串和數值沒有初始字符，`ng-class` 不會給它們分配任何類別，所以 CSS 可以利用預設樣式，來讓它們向右對齊。

最後，我們使用 `</td>` 來離開直欄的 `ng-repeat` 迴圈，再使用`</tr>`  離開橫列迴圈，並結束 HTML 文件：

```html
    </td>
  </tr></table>
</body></html>
```

### JS: 主要控制層

`main.js` 的唯一作用，是定義 `<body>` 元件所需的 `Spreadsheet` 控制函式，其中利用 AngularJS 提供的 `$scope` 參數來定義 JS 模型：

```js
function Spreadsheet ($scope, $timeout) {
```

`$scope` 中的 `$` 是變數名稱的一部分。我們也向 AngularJS 要求 [`$timeout`](https://docs.angularjs.org/api/ng/service/$timeout) 服務函式；稍後我們會運用它來避免公式進入無限循環。

要把 `Cols` 和 `Rows` 放進模型，直接將它們定義為 `$scope` 的屬性即可：

```js
  // Begin of $scope properties; start with the column/row labels
  $scope.Cols = [ for (col of range( 'A', 'H' )) col ];
  $scope.Rows = [ for (row of range( 1, 20 )) row ];
```

使用 ES6 的[陣列簡約式](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Array_comprehensions)語法，可以很容易將陣列定義為從起點到終點的範圍。這裡用到了 `range` 這個[產生器](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function*) 函式作為輔助：

```js
  function* range(cur, end) { while (cur <= end) { yield cur;
```

The `function*` above means that `range` returns an [iterator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/The_Iterator_protocol), with a `while` loop that would  [`yield`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/yield) a single value at a time. Whenever the `for` loop demands the next value, it will resume execution right after the `yield` line:

上述的 `function*` 語法，會讓 `range` 傳回[迭代器](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/The_Iterator_protocol)，其中的 `while` 迴圈每次 `yield` 一個值。每當 `for` 需要下一個數值時，迴圈就會從上次的 `yield` 後面接續執行：

```
    // If it’s a number, increase it by one; otherwise move to next letter
    cur = (isNaN( cur ) ? String.fromCodePoint( cur.codePointAt()+1 ) : cur+1);
  } }
```

要產生下一個數值時，我們使用 `isNaN` 來查看 `cur` 是否為英文字母（`NaN` 代表「非數字」，not a number）。如果是，我們可以將字母的[碼位值](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/codePointAt)加一，然後將碼位[轉換](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCodePoint)成下一個字母。如果 `cur` 是數字，那只要加一就可以了。

接著我們定義 `keydown()` 函式，來處理鍵盤往上下列的移動事件：

```js
  // UP(38) and DOWN(40)/ENTER(13) move focus to the row above (-1) and below (+1).
  $scope.keydown = ({which}, col, row)=>{ switch (which) {
```

[箭號函數](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/arrow_functions) 從 `<input ng-keydown>` 接收引數 `($event, col, row)` 之後，使用 [解構賦值](https://developer.mozilla.org/en-US/docs/Web/JavaScript/New_in_JavaScript/1.7#Pulling_fields_from_objects_passed_as_function_parameter) 將 `$event.which` 指派到 `which` 參數裡，並檢查它是否屬於要處理的三種按按鍵碼之一：

```js
    case 38: case 40: case 13: $timeout( ()=>{
```

如果是的話，我們就用 `$timeout` 將「更新儲存格焦點」加入排程，在 `ng-keydown` 和 `ng-change` 事件後接著執行。因為 `$timeout` 的引數必須是函式，我們用 `()=>{…}` 語法將更新焦點的邏輯包進函式裡。首先檢查移動方向：

```js
      const direction = (which === 38) ? -1 : +1;
```

此處的 `const` 宣告，指的是 `direction` 的值在函式執行期間不會改變。如果按鍵碼是**上鍵** (38)，表示往上移動（`-1`， 從 **A2** 到 **A1**），否則往下移動（`+1`，從 **A2** 到 **A3**）。

接下來，我們使用以反引號撰寫的[模板字串](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/template_strings) 將開頭的 `#`、當前的直欄 `col` 和目標橫列 `row + direction` 串聯在一起，來構築 ID 選取器（例如`"#A3”`），以取得目標元素：

```js
      const cell = document.querySelector( `#${ col }${ row + direction }` );
      if (cell) { cell.focus(); }
    } )
  } };
```

之所以要檢查 `querySelector` 是否有傳回值，是因為從 **A1** 向上移動時，會產生選取器 `#A0`：它沒有相應的元件，因此不會觸發焦點變化。在最底端的橫列按**下鍵**時也是一樣。

接著我們定義 `reset()` 函數，讓 `↻` 按鈕可以恢復 `sheet` 的初始內容：

```js
  // Default sheet content, with some data cells and one formula cell.
  $scope.reset = ()=>{ $scope.sheet = { A1: 1874, B1: '+', C1: 2046, D1: '⇒', E1: '=A1+C1' } }
```

`init()` 函式先試著從 [localStorage](https://developer.mozilla.org/en-US/docs/Web/Guide/API/DOM/Storage#localStorage) 恢復 `sheet` 內容的先前狀態。或如果是首次運行，則預設為初始內容：

```js
  // Define the initializer, and immediately call it
  ($scope.init = ()=>{
    // Restore the previous .sheet; reset to default if it’s the first run
    $scope.sheet = angular.fromJson( localStorage.getItem( '' ) );
    if (!$scope.sheet) { $scope.reset(); }
    $scope.worker = new Worker( 'worker.js' );
  })();
```

上述的 `init()` 函式裡，有幾件事情值得注意：

* 我們使用 `($scope.init = ()=>{…})()` 語法，在函式定義之後立刻執行它。
* 由於 localStorage 只能存取字串，所以要用 `angular.fromJson()` 將 [JSON](https://developer.mozilla.org/en-US/docs/Glossary/JSON) 解析（parse）為 `sheet` 結構。
* `init()` 的最後一步，是建立新的 [Web Worker](https://developer.mozilla.org/en-US/docs/Web/API/Worker) 線程，並將它放進 `worker` 屬性裡。雖然顯示層不會直接用到背景工作器，但我們仍依慣例使用 `$scope` 屬性，來讓多個函式能共用同一個模型裡的物件（此處指的是 `init()` 和下述的 `calc()` ）。

`sheet` 儲存了使用者可編輯的儲存格內容，而 `errs` 和 `vals` 則包含了計算的結果——錯誤和數值——兩者對於使用者來說都屬於唯讀內容：

```js
  // Formula cells may produce errors in .errs; normal cell contents are in .vals
  [$scope.errs, $scope.vals] = [ {}, {} ];
```

有了這些屬性，就可以定義出每當使用者變更 `sheet` 時，都會觸發的 `calc()` 函式：

```js
  // Define the calculation handler, and immediately call it
  ($scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet );
```

首先是把 `sheet` 的狀態快照成 JSON 字串，儲存在常數 `json` 裡。

接著我們用 [$timeout](https://docs.angularjs.org/api/ng/service/$timeout) 建構一個 `promise` ，如果接下來的計算需時超過 99 毫秒，就自動將它取消：

```js
    const promise = $timeout( ()=>{
      // If the worker has not returned in 99 milliseconds, terminate it
      $scope.worker.terminate();
      // Back up to the previous state and make a new worker
      $scope.init();
      // Redo the calculation using the last-known state
      $scope.calc();
    }, 99 );
```

在 HTML 中的 `<input ng-model-options>` 屬性，確保 `calc` 最多只會每 200 毫秒執行一次，因此這裡預留了 101 毫秒，讓 `init()` 恢復 `sheet` 到上一個已知的良好狀態，並建立新的背景工作器。

工作器的任務，是從 `sheet` 的內容計算出 `errs` 和 `vals`。由於 **main.js** 和 **worker.js** 是透過訊息傳遞來溝通，所以需要定義一個 `onmessage` 處理程序，來接收計算出的結果：

```js
    // When the worker returns, apply its effect on the scope
    $scope.worker.onmessage = ({data})=>{
      $timeout.cancel( promise );
      localStorage.setItem( '', json );
      $timeout( ()=>{ [$scope.errs, $scope.vals] = data; } );
    }
```

一旦 `onmessage` 開始執行，就表示 `json` 中的 `sheet` 快照是穩定的（也就是說公式裡沒有無限迴圈），因此我們可以取消 99 毫秒的逾時限制，把快照寫進 localStorage 裡，並使用 `$timeout` 函式將界面更新加入排程，把 `errs` 和 `vals` 更新到顯示層、呈現給使用者。

定義完處理程序之後，就可以將 `sheet` 的狀態傳送給工作者，在背景開始運算處理：

```js
    // Post the current sheet content for the worker to process
    $scope.worker.postMessage( $scope.sheet );
  })();
}
```

### JS: 背景工作者

There are three reasons for using a Web Worker to calculate formulas, instead of using the main JS thread for the task:

* While the worker runs in the background, the user is free to continue interacting with the spreadsheet, without getting blocked by computation in the main thread. 
* Because we accept any JS expression in a formula, the worker provides a _sandbox_ that prevents formulas from interfering with the page that contains them, such as by popping out an `alert()` dialog.
* A formula can refer to any coordinates as variables, which may contain yet another formula that possibly ends in a cyclic reference. To solve this problem, we use the Worker’s _global scope_ object `self`, and define these variables as _getter functions_ on `self` to implement the cycle-prevention logic.

With these in mind, let’s take a look at the worker’s code. Because **index.html** pre-loads the worker program with a `<script>` tag, in **worker.js** we first ensure that we’re actually running as a background task:

```js
if (self.importScripts) {
```

This check works because the browser defines `importScripts()` only in a Worker thread.

The Worker’s sole purpose is defining its `onmessage` handler that takes `sheet`, calculates `errs` and `vals`, and posts them back to the main JS thread. We begin by re-initializing the three variables when we receive a message:

```js
  let sheet, errs, vals;
  self.onmessage = ({data})=>{
    [sheet, errs, vals] = [ data, {}, {} ];
```

In order to turn coordinates into global variables, we first iterate over each property in `sheet`, using a `for…in` loop:

```js
    for (const coord in sheet) {
```

We  write `const coord` above so that functions defined in the loop can capture the specific value of `coord` in that iteration. This is because `const` and `let` declare _block scoped_ variables. In contrast, `var coord` would make a _function scoped_ variable, and functions defined in each loop iteration would end up pointing to the same `coord`.

Customarily, formulas variables are case-insensitive and can optionally have a `$` prefix. Because JS variables are case-sensitive, we use a `for…of` loop to go over the four variable names for the same coordinate:

```js
      // Four variable names pointing to the same coordinate: A1, a1, $A1, $a1
      for (const name of [ for (p of [ '', '$' ])
                             for (c of [ coord, coord.toLowerCase() ])
                               p+c ]) {
```

Note the _nested array comprehension_ syntax above, with  two `for`  expressions in the array definition.

For each variable name like `A1` and `$a1`, we define its [accessor property](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty) on `self` that calculates `vals[A1]` whenever they are evaluated in an expression:

```js
        // Worker is reused across calculations, so only define each variable once
        if ((Object.getOwnPropertyDescriptor( self, name ) || {}).get) { continue; }

        // Define self['A1'], which is the same thing as the global variable A1
        Object.defineProperty( self, name, { get() {
```

The `{ get() { … } }` syntax above is shorthand for `{ get: ()=>{ … } }`. Because we define only `get` and not `set`, the variables become  _read-only_ and cannot be modified from user-supplied formulas.

The `get` accessor starts by checking if `vals[coord]` is already calculated, and simply returns it if it’s the same:

```js
          if (coord in vals) { return vals[coord]; }
```

If not, we need to calculate `vals[coord]` from `sheet[coord]`.

First we set it to `NaN`, so self-references like setting **A1*** to `=A1` will end up with `NaN` instead of an infinite loop:

```js
          vals[coord] = NaN;
```

Next we check if `sheet[coord]` is a number by converting it to numeric with prefix `+`, assigning the number to `x`, and comparing its string representation with the original string. If they differ, then we set `x` to the original string:

```js
          // Turn numeric strings into numbers, so =A1+C1 works when both are numbers
          let x = +sheet[coord];
          if (sheet[coord] !== x.toString()) { x = sheet[coord]; }
```

If the initial character of `x` is `=`, then it’s a formula cell. We evaluate the part after `=` with `eval.call()`, using the first argument `null` to tell `eval` to run in the _global scope_, hiding the _lexical scope_ variables like `x` and `sheet` from the evaluation:

```js
          // Evaluate formula cells that begin with =
          try { vals[coord] = (('=' === x[0]) ? eval.call( null, x.slice( 1 ) ) : x);
```

If the evaluation succeeds, the result is stored into `vals[coord]`. For non-formula cells, the value of `vals[coord]` is simply `x`, which may be a number or a string.

If `eval` results in an error, the `catch` block tests if it’s because the formula refers to an empty cell not yet defined in `self`:

```js
          } catch (e) {
            const match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec( e );
            if (match && !( match[0] in self )) {
```

In that case, we set the missing cell’s default value to `0`, clear `vals[coord]`, and re-run the current computation using `self[coord]`:

```js
              // The formula refers to a uninitialized cell; set it to 0 and retry
              self[match[0]] = 0;
              delete vals[coord];
              return self[coord];
            }
```

 If the user gives the missing cell a content later on in `sheet[coord]`, then `Object.defineProperty` would take over and override the temporary value.

Other kinds of errors are stored to `errs[coord]`:

```js
            // Otherwise, stringify the caught exception in the errs object
            errs[coord] = e.toString();
          }
```

In case of errors, the value of `vals[coord]` will remain `NaN` because the assignment did not complete.

Finally, the `get` accessor returns the calculated value, stored in `vals[coord]`:

```js
          return vals[coord];
        } } )
      }
    }
```

With accessors defined for all coordinates, the worker goes through the coordinates again, invoking each accessor by accessing `self[coord]`, then posts the resulting `errs` and `vals` back to the main JS thread:

```js
    // For each coordinate in the sheet, call the property getter defined above
    for (const coord in sheet) { self[coord]; }
    return [ errs, vals ];
  }
}
```

### CSS

The **styles.css** file contains just a few selectors and their presentational styles. First, we style the table to merge all cell borders together, leaving no spaces between neighboring cells:

```css
table { border-collapse: collapse; }
```

Both the heading and data cells share the same border style, but we can tell them apart by their background colors: Heading cells are light-gray, data cells are white by default, and formula cells get a light-blue background:

```
th, td { border: 1px solid #ccc; }
th { background: #ddd; }
td.formula { background: #eef; }
```

The displayed width is fixed for each cell’s calculated values. Empty cells receive a minimal height, and long lines are clipped with a trailing ellipsis:

```css
td div { text-align: right; width: 120px; min-height: 1.2em;
         overflow: hidden; text-overflow: ellipsis; }
```

The text alignment and decorations are determined by each value’s type, as reflected by the `text` and `error` class selectors:

```css
div.text { text-align: left; }
div.error { text-align: center; color: #800; font-size: 90%; border: solid 1px #800 }
```

As for the user-editable `input` box, we use _absolute positioning_ to overlay it on top of its cell, and make it transparent so the underlying `div` with the cell’s value shows through:

```css
input { position: absolute; border: 0; padding: 0;
        width: 120px; height: 1.3em; font-size: 100%;
        color: transparent; background: transparent; }
```

When the user sets focus on the input box, it springs into the foreground:

```css
input:focus { color: #111; background: #efe; }
```

Furthermore, the underlying `div` is collapsed into a single line, so it’s completely covered by the input box:

```css
input:focus + div { white-space: nowrap; }
```

## 總結

Since this book is _500 lines or less_, a web spreadsheet in 99 lines is just a minimal example — please feel free to experiment and extend it in any direction you’d like.

Here are some ideas, all easily reachable in the remaining space of 401 lines:

* A collaborative online editor using [ShareJS](http://sharejs.org/), [AngularFire](http://angularfire.com) or [GoAngular](http://goangular.org/).
* Markdown syntax support for text cells, using [angular-marked](http://ngmodules.org/modules/angular-marked).
* Common formula functions (`SUM`, `TRIM`, etc.) from the [OpenFormula standard](https://en.wikipedia.org/wiki/OpenFormula).
* Interoperate with popular spreadsheet formats, such as CSV and SpreadsheetML via [SheetJS](http://sheetjs.com/)
* Import from and export to online spreadsheet services, such as Google Spreadsheet and [EtherCalc](http://ethercalc.net/).

### 備註: 關於 JS 版本

This chapter aims to demonstrate new concepts in ES6, so we use the [Traceur compiler](https://github.com/google/traceur-compiler) to translate source code to ES5 to run on pre-2015 browsers.

If you prefer to work directly with the 2010 edition of JS, the [as-javascript-1.8.5](https://audreyt.github.io/500lines/spreadsheet/as-javascript-1.8.5/) directory has **main.js** and **worker.js** written in the style of ES5; the [source code](https://github.com/audreyt/500lines/tree/master/spreadsheet/as-javascript-1.8.5) is line-by-line comparable to the ES6 version with the same line count.

For people preferring a cleaner syntax, the [as-livescript-1.2.0](https://audreyt.github.io/500lines/spreadsheet/as-livescript-1.2.0/) directory uses [LiveScript](http://livescript.net/) instead of ES6 to write **main.ls** and **worker.ls**; the [source code](https://github.com/audreyt/500lines/tree/master/spreadsheet/as-livescript-1.2.0) is 20 lines shorter than the JS version.

If you are interested in translating this example to alternate JS languages, send a [pull request](https://github.com/audreyt/500lines/pulls) — I’d love to hear about it!
