_(Also available in [English](https://github.com/audreyt/500lines/blob/master/spreadsheet/chapter.md). The ES5 version is available as a [jsFiddle](http://jsfiddle.net/audreyt/LtDyP/).)_

# 用 [99 行程式](https://github.com/audreyt/500lines/tree/master/spreadsheet)寫出[網頁試算表](http://audreyt.github.io/500lines/spreadsheet/)

本章介紹以 99 行 HTML、CSS 和 JS 程式（瀏覽器原生支援的三種語言）寫成的試算表網頁應用。

## 簡介

1990 年，Tim Berners-Lee 發明了全球資訊網，當時的網頁文件（Web pages）都是以 HTML 寫成，使用尖括號的標籤（tags）來標記文字，給內容安排邏輯結構。以 `<a>…</a>` 標記的文字會變成超連結（hyperlinks），把使用者導引至其他網頁。

在 20 世紀 90 年代，瀏覽器加入了各種展示性標籤到 HTML 詞彙，包括一些聲名狼藉的非標準標籤，例如來自 Netscape Navigator 的 `<blink>…</blink>` 和來自 Internet Explorer 的 `<marquee>…</marquee>` ，都造成了親和力和瀏覽器兼容性的廣泛問題。

為了保持 HTML 的原本目的（描述文件的邏輯結構），瀏覽器開發者最後同意額外支援兩種語言：CSS 來形容網頁的展示風格，以及 JS 來描述其動態互動功能。

從那時開始，這三種程式語言經過了 20 年的共同進化，已經變得更加簡潔和強大。[JS 引擎](https://zh.wikipedia.org/wiki/JavaScript%E5%BC%95%E6%93%8E)的效能獲得高度提昇，使得大規模的 JS 框架開始盛行，例如 [AngularJS](http://angularjs.org/)。

如今，跨平台的應用網站（Web applications，例如試算表），已經跟上個世紀的桌面應用程式（如 VisiCalc、Lotus 1-2-3 和 Excel）一樣普及了。

使用 AngularJS 的網頁應用可以在 99 行裡面提供多少功能？讓我們來看看！

## 概述

在 [spreadsheet](https://github.com/audreyt/500lines/tree/master/spreadsheet) 目錄裡，包含了三種 Web 程式語言在 2014 年末版本的展示範例：描述結構的 [HTML5](http://www.w3.org/TR/html5/)、描述展示風格的 [CSS3](http://www.w3.org/TR/css3-ui/)，以及描述互動功能的 JS [ES6 “Harmony”](http://git.io/es6features) 。它也用到 [Web Storage](http://www.whatwg.org/specs/web-apps/current-work/multipage/webstorage.html) 來保存資料，以及利用 [Web Worker](http://www.whatwg.org/specs/web-apps/current-work/multipage/workers.html) 在背景運行 JS 程式碼。在撰寫本文時，這些 Web 標準都已獲得 Firefox、Chrome、Internet Explorer 11+，以及手機行動瀏覽器 iOS 5+ 和 Android 4+ 的支援。

現在讓我們在瀏覽器中打開 <http://audreyt.github.io/500lines/spreadsheet/>：

![初始畫面](./spreadsheet-images/01-initial.png)

### 基本概念

試算表往兩個方向延伸，直欄（columns）由 **A** 開始編號，而橫列（rows）則由 **1** 開始。每個儲存格（cell）都有各自的座標（coordinate）與內容（content），例如 **A1** 與 `1874`。內容的型別（types）有以下四種：

* 文字：**B1** 中的 `+` 和 **D1** 中的 `⇒`，向左對齊。
* 數字：**A1** 中的 `1874` 和 **C1** 中的 `2046`， 向右對齊。
* 公式：**E1** 中的 `=A1+C1`，經計算（calculation）後得出數值（value）為 `3920`，以淡藍色背景顯示。
* 空白：橫列 **2** 上的所有儲存格目前沒有內容。

點擊 `3920` 來移動焦點（focus）到 **E1**，會出現顯示公式的輸入框（input box）：

![輸入框](./spreadsheet-images/02-input.png)

現在把選取焦點移到 **A1** ，將內容變更（change）為 `1`，使 **E1** 的數值重新計算（recalculate）成 `2047`：

![已變更的內容](./spreadsheet-images/03-changed.png)

按 **輸入鍵（ENTER）** 以設定焦點至 **A2**，並變更其內容為 `=Date()`，然後按 **跳格鍵（TAB）**，變更 **B2** 的內容為 `=alert()`，然後再按 **跳格鍵** 以移動焦點至 `C2`：

![公式錯誤](./spreadsheet-images/04-error.png)

以上展示了公式的計算結果可以是數字（**E1** 中的 `2047`）、文字（**A2** 顯示的目前時間，向左對齊），或者是錯誤（error，如 **B2** 中的紅色字母，置中對齊）。

接下來試試輸入 `=for(;;){}`，這是一段 JS 程式碼，會執行永無休止的無窮迴圈。試算表會在嘗試變更時，自動回復（restore） **C2** 的內容，以避免執行這段程式。

現在按 **Ctrl-R** 或 **Cmd-R** 來重新載入瀏覽器頁面，確認試算表的內容能夠延續（persistent），在每個瀏覽階段中保持一致。 如果要重置（reset）試算表的原始內容，可按左上角的 `↻` 按鈕。

### 漸進增強

在我們開始詳細討論 99 行程式碼之前，不妨先停用瀏覽器中的 JS ，並重新載入頁面，注意一下前後區別：

* 大型表格消失了，螢幕上只剩下一個 2x2 的表格以及單個內容儲存格。
* 列和欄的標題變成 `{{ row }}` 和 `{{ col }}`。
* 按下 `↻` 按鈕沒有任何反應。
* 按 **跳格鍵** 或點擊第一列的內容時，仍然會顯示可編輯的輸入框。

![停用 JavaScript](./spreadsheet-images/05-nojs.png)

當我們停用動態互動功能（JS）時，內容結構（HTML）和展示風格（CSS）依然持續生效。如果網頁文件能夠在 JS 和 CSS 同時停用的情況下，繼續保持可用，就表示它遵守「漸進增強」（progressive enhancement）原則，讓內容能接觸到最廣的使用者群。

由於我們的試算表是應用網頁，並沒有伺服器端的程式碼，所以我們必須依賴 JS 來提供所需要的邏輯。然而，即使 CSS 沒有完全獲得支援，比如說在螢幕閱讀器和文字模式的瀏覽器裡，程式依然能正常運作。

![停用 CSS](./spreadsheet-images/06-nocss.png)

如同以上畫面顯示，如果我們在瀏覽器中啟用 JS ，但轉而停用 CSS 的話，效果如下：

* 所有背景和前景顏色會消失。
* 輸入框和儲存格的數值會同時顯示，而不只是顯示其中一個。
* 除此之外，網頁應用完全正常運作。

## 程式碼逐步解說

以下的架構圖描繪 HTML 和 JS 元件之間的各式連結：

![架構圖](./spreadsheet-images/00-architecture.png)

為了理解這張圖，讓我們按照瀏覽器載入的次序，逐一檢視四份原始程式碼檔案：

* **index.html**: 19 行
* **main.js**: 38 行（不包括註釋和空白行）
* **worker.js**: 30 行（不包括註釋和空白行）
* **styles.css**: 12 行

### HTML

`index.html` 的第一行，聲明這是使用 HTML5（`<!DOCTYPE html>`） 與 UTF-8 編碼寫成的網頁：

```html
<!DOCTYPE html><html><head><meta charset="UTF-8">
```

如果沒有宣告 `charset` ，瀏覽器可能會把重置按鈕的萬國碼符號 `↻` 顯示為 `â†»`，也就是亂碼（mojibake）──由解碼問題引起的錯誤情況。

接下來的三行是 JS 宣告，依慣例放在 `head` 區塊裡：

```html
  <script src="lib/angular.js"></script>
  <script src="main.js"></script>
  <script>try{ angular.module('500lines') }catch(e){ location="es5/index.html" }</script>
```

標籤 `<script src="…">` 在和 HTML 頁面相同的路徑下載入 JS 資源。舉例來說，如果目前的網址是 `http://audreyt.github.io/500lines/spreadsheet/index.html`，那麼 `lib/angular.js` 會指向 `http://audreyt.github.io/500lines/spreadsheet/lib/angular.js`。

`try{ angular.module('500lines') }` 這行程式碼用於測試 `main.js` 是否已經正確載入；如果沒有正確載入，會將瀏覽器轉至 `es5/index.html`。對於 2015 年以前發行、並且不支援 ES6 的瀏覽器來說，這個「優雅降級」（graceful degradation）的重新導向技巧，確保我們可以用轉譯到 ES5 版本的 JS 程式，來作為備用方案。

接下來的兩行程式碼會載入 CSS 資源，結束 `head` 部分，並開始 `body` 部分，當中包含使用者會看到的內容：

```html
  <link href="styles.css" rel="stylesheet">
</head><body ng-app="500lines" ng-controller="Spreadsheet" ng-cloak>
```

上述的 `ng-` 特性指示 [AngularJS](http://angularjs.org/) 運行 `500lines` 模組中的 `Spreadsheet` 控制器（controller）函式來取得模型（model）物件，來為文件顯示層（view）提供繫結（binding）。（`ng-cloak` 特性會先隱藏文件，等到繫結已經就位再顯示。）

舉個具體的例子，當使用者點擊下一行中所定義的 `<button>` ，其 `ng-click` 特性會觸發並執行 `reset()` 和 `calc()` 這兩個由 JS 模型提供的函式：

```html
  <table><tr>
    <th><button type="button" ng-click="reset(); calc()">↻</button></th>
```

下一行使用 `ng-repeat` ，在置頂列顯示各個直欄的標籤：

```html
    <th ng-repeat="col in Cols">{{ col }}</th>
```

舉例來說，如果 JS 模型將 `Cols` 定義為 `["A","B","C"]`，就會出現三個標題儲存格（`th`）。內容的 `{{ col }}` 表達式會由 AngularJS 進行安插（interpolation），來在每個 `th` 的內容中填上當前的 `col` 數值。

同樣地，下面兩行會檢查 `Rows` 的數值（`[1,2,3]` 等等），為每個數值建立橫列，並在最左邊的 `th` 儲存格以編號標註：

```html
  </tr><tr ng-repeat="row in Rows">
    <th>{{ row }}</th>
```

由於使用 `<tr ng-repeat>` 標籤開始的程式碼還沒有被 `</tr>` 結束，表達式可以繼續使用 `row` 變數。下一行程式碼，在當前的橫列上建立一個資料儲存格（`td`），並在其 `ng-class` 特性中同時使用 `col` 和 `row` 變數：

```html
    <td ng-repeat="col in Cols" ng-class="{ formula: ( '=' === sheet[col+row][0] ) }">
```

這裡有幾個重點。在 HTML 中，`class` 特性描述類別名稱的集合（a set of class names），讓 CSS 賦予它們不同的樣式。這裡的 `ng-class` 會運算表達式 `( '=' === sheet[col+row][0] )`；如果結果為真，那麼 `<td>` 會獲得類別 `formula` ，因此儲存格就會添上淡藍色背景，由 **styles.css** 第 4 行中的 `.formula`「類別選擇器」（class selector）所定義。

上述表達式用來檢查目前儲存格是否為公式的方法，是透過測試 `=` 是否為 `sheet[col+row]` 字串的初始字符（`[0]`）。此處 `sheet` 是 JS 模型物件，屬性為各個座標（例如 `"E1"`），儲存格內容（例如 `"=A1+C1"`）則是屬性的值。要注意的是，由於 `col` 是字串而非數字，因此 `col+row` 中的 `+` 指的是串聯，而不是加法。

在 `<td>` 中，我們給使用者一個輸入框，來編輯儲存在 `sheet[col+row]` 中的儲存格內容：

```html
      <input id="{{ col+row }}" ng-model="sheet[col+row]" ng-change="calc()"
       ng-model-options="{ debounce: 200 }" ng-keydown="keydown( $event, col, row )">
```

這裡的主要特性是 `ng-model`，它允許 JS 模型和輸入框內可編輯內容之間的「雙向繫結」（two-way binding）：每當使用者在輸入框內做出變更時，JS 模型都會自動更新 `sheet[col+row]` 的內容，並觸發 `calc()` 函式來重新計算所有公式儲存格的數值。

當使用者按住某個鍵不放的時候，為了避免重複執行 `calc()`，`ng-model-options` 會限制更新速率至每 200 毫秒一次。

輸入框的 `id` 特性與座標相同，由 `col+row` 安插取得。每個 HTML 元素的 `id` 特性，必須跟文件內所有其他元素的 `id` 不同。這樣確保了 `#A1` 這個「ID 選擇器」（ID selector）只會指向至一個元素，而不像類別選擇器 `.formula` 那樣指稱一系列的元素。當使用者按下 **上鍵**/**下鍵**/**輸入鍵** 時，`keydown()` 中的邏輯就會使用 ID 選擇器，來確定該設定焦點在哪個輸入框上。

在輸入框後面，我們放置一個 `<div>` 元素，來顯示當前儲存格的計算結果（以物件 `errs` 和 `vals` 在 JS 模型中表示）：

```html
      <div ng-class="{ error: errs[col+row], text: vals[col+row][0] }">
        {{ errs[col+row] || vals[col+row] }}</div>
```

如果計算公式發生錯誤，文字插值會使用 `errs[col+row]` 裡存放的錯誤訊息，而 `ng-class` 會套用 `error` 類別至該元素，為它套用特定的 CSS 樣式（使用紅色文字、置中對齊等等）。

如果沒有錯誤，進行安插的會是 `||` 右側的 `vals[col+row]`。如果它是一個非空白的字符串，初始字符（`[0]`）就會運算為真，使得 `text` 類別套用到元素上，使文字向左對齊。

由於空白字符串和數值沒有初始字符，`ng-class` 不會給它們分配任何類別，所以 CSS 可以利用預設樣式，來讓它們向右對齊。

最後，我們使用 `</td>` 來離開直欄的 `ng-repeat` 迴圈，再使用 `</tr>` 離開橫列迴圈，並結束 HTML 文件：

```html
    </td>
  </tr></table>
</body></html>
```

### JS: 主要控制層

`main.js` 定義了 `index.html` 中 `<body>` 元素所需的 `500lines` 模組，以及模組內的 `Spreadsheet` 控制函式。

作為 HTML 文件顯示層與背景工作層之間的橋梁，控制層有四項任務：

* 定義各欄、各列的數量與標題。
* 為鍵盤移動事件及重置按鈕提供處理函式。
* 當使用者更動試算表式，將新的內容傳送給背景工作者。
* 當工作者計算出結果時，更新文件顯示層，並儲存目前的狀態。

控制層和工作層之間的互動詳情，可以參考這張流程圖：

![控制層和工作層之間的互動](./spreadsheet-images/00-flowchart.png)

現在來看程式碼。在第一行程式裡，我們向 AngularJS 要求 `$scope` 物件，來定義 JS 模型：

```js
angular.module('500lines', []).controller('Spreadsheet', function ($scope, $timeout) {
```

`$scope` 中的 `$` 是變數名稱的一部分。我們也向 AngularJS 要求 [`$timeout`](https://docs.angularjs.org/api/ng/service/$timeout) 服務函式；稍後我們會運用它來避免公式進入無限循環。

要把 `Cols` 和 `Rows` 放進模型，直接將它們定義為 `$scope` 的屬性即可：

```js
  // Begin of $scope properties; start with the column/row labels
  $scope.Cols = [], $scope.Rows = [];
  for (col of range( 'A', 'H' )) { $scope.Cols.push(col); }
  for (row of range( 1, 20 )) { $scope.Rows.push(row); }
```

此處使用 ES6 的 [for...of](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...of) 迴圈語法，來逐項處理起點到終點的範圍，並使用 `range` 這個[產生器](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function*)函式作為輔助：

```js
  function* range(cur, end) { while (cur <= end) { yield cur;
```

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

[箭號函式](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/arrow_functions)從 `<input ng-keydown>` 接收引數 `($event, col, row)` 之後，使用[解構賦值](https://developer.mozilla.org/en-US/docs/Web/JavaScript/New_in_JavaScript/1.7#Pulling_fields_from_objects_passed_as_function_parameter)將 `$event.which` 指派到 `which` 參數裡，並檢查它是否屬於要處理的三種鍵碼之一：

```js
    case 38: case 40: case 13: $timeout( ()=>{
```

如果是的話，我們就用 `$timeout` 將「更新儲存格焦點」加入排程，在 `ng-keydown` 和 `ng-change` 事件後接著執行。因為 `$timeout` 的引數必須是函式，我們用 `()=>{…}` 語法將更新焦點的邏輯包進函式裡。首先檢查移動方向：

```js
      const direction = (which === 38) ? -1 : +1;
```

此處的 `const` 宣告，指的是 `direction` 的值在函式執行期間不會改變。如果按鍵碼是 38（**上鍵**），表示往上移動（`-1`， 從 **A2** 到 **A1**），否則往下移動（`+1`，從 **A2** 到 **A3**）。

接下來，我們使用以反引號撰寫的[模板字串](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/template_strings)將開頭的 `#`、當前的直欄 `col` 和目標橫列 `row + direction` 串聯在一起，構築出 ID 選擇器（例如`"#A3"`），來取得目標元素：

```js
      const cell = document.querySelector( `#${ col }${ row + direction }` );
      if (cell) { cell.focus(); }
    } )
  } };
```

之所以要檢查 `querySelector` 是否有傳回值，是因為從 **A1** 向上移動時，會產生選擇器 `#A0`：它沒有相應的元素，因此不會觸發焦點變化。在最底端的橫列按 **下鍵** 時也是一樣。

接著我們定義 `reset()` 函式，讓 `↻` 按鈕可以恢復 `sheet` 的初始內容：

```js
  // Default sheet content, with some data cells and one formula cell.
  $scope.reset = ()=>{ $scope.sheet = { A1: 1874, B1: '+', C1: 2046, D1: '⇒', E1: '=A1+C1' } }
```

`init()` 函式先試著從 [localStorage](https://developer.mozilla.org/en-US/docs/Web/Guide/API/DOM/Storage#localStorage) 恢復 `sheet` 內容的先前狀態。如果是首次運行，則預設為初始內容：

```js
  // Define the initializer, and immediately call it
  ($scope.init = ()=>{
    // Restore the previous .sheet; reset to default if it’s the first run
    $scope.sheet = angular.fromJson( localStorage.getItem( '' ) );
    if (!$scope.sheet) { $scope.reset(); }
    $scope.worker = new Worker( 'worker.js' );
  }).call();
```

上述的 `init()` 函式裡，有幾件事情值得注意：

* 我們使用 `($scope.init = ()=>{…}).call()` 語法，在函式定義之後立刻執行它。
* 由於 localStorage 只能存取字串，所以要用 `angular.fromJson()` 將 [JSON](https://developer.mozilla.org/en-US/docs/Glossary/JSON) 解析（parse）為 `sheet` 結構。
* `init()` 的最後一步，是建立新的 [Web Worker](https://developer.mozilla.org/en-US/docs/Web/API/Worker) 線程，並將它放進 `worker` 屬性裡。雖然顯示層不會直接用到背景工作器，但我們仍依慣例使用 `$scope` 的屬性，來讓多個函式能共用同一個模型裡的物件（此處指的是 `init()` 和下述的 `calc()` ）。

`sheet` 儲存了使用者可編輯的儲存格內容，而 `errs` 和 `vals` 則是計算的結果。錯誤和數值兩者，對使用者來說都是唯讀的：

```js
  // Formula cells may produce errors in .errs; normal cell contents are in .vals
  [$scope.errs, $scope.vals] = [ {}, {} ];
```

有了這些屬性，就可以定義出每當使用者變更 `sheet` 時，就會觸發的 `calc()` 函式：

```js
  // Define the calculation handler; not calling it yet
  $scope.calc = ()=>{
    const json = angular.toJson( $scope.sheet );
```

首先是把 `sheet` 的狀態快照成 JSON 字串，存進常數 `json` 裡。

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

在 HTML 中的 `<input ng-model-options>` 特性，確保 `calc` 最多只會每 200 毫秒執行一次，因此這裡預留了 101 毫秒，讓 `init()` 恢復 `sheet` 到上一個已知的良好狀態，並建立新的背景工作器。

工作器的任務，是從 `sheet` 的內容計算出 `errs` 和 `vals`。由於 **main.js** 和 **worker.js** 是透過訊息傳遞來溝通，所以需要定義一個 `onmessage` 處理程序，來接收計算出的結果：

```js
    // When the worker returns, apply its effect on the scope
    $scope.worker.onmessage = ({data})=>{
      $timeout.cancel( promise );
      localStorage.setItem( '', json );
      $timeout( ()=>{ [$scope.errs, $scope.vals] = data; } );
    }
```

一旦 `onmessage` 開始執行，就表示 `json` 中的 `sheet` 快照是穩定的（也就是說公式裡沒有無限迴圈），因此我們可以取消 99 毫秒的逾時限制、把快照寫進 localStorage 裡，並使用 `$timeout` 函式將界面更新加入排程，將 `errs` 和 `vals` 更新到顯示層呈現給使用者。

定義好處理程序之後，就可以將 `sheet` 的狀態傳送給工作者，在背景開始運算：

```js
    // Post the current sheet content for the worker to process
    $scope.worker.postMessage( $scope.sheet );
  };

  // Start calculation when worker is ready
  $scope.worker.onmessage = $scope.calc;
  $scope.worker.postMessage( null );
});
```

### JS: 背景工作

計算公式不在 JS 主要線程中處理，而是使用背景工作，有三個原因：

* 當工作者在背景執行時，使用者可以繼續操作試算表界面，不會被主線程的運算程序阻礙。
* 因為公式裡可以出現任何 JS 表達式，工作者為此提供了沙盒（sandbox），來防止公式干擾到主線程的網頁，例如使用 `alert()` 彈出對話框等等。
* 公式可以用任何座標當作變數，該變數可能包含另一項公式，最終可能導致循環引用。為了解決這個問題，我們利用工作者的全域範圍（global scope）物件 `self`，將各座標變數定義成它的取值函式（getter function），以實作防止循環的邏輯。

有了這些認識後，讓我們來看看工作者的程式碼。

工作者的唯一目的是定義 `onmessage` 處理程序，來接收 `sheet`、計算出 `errs` 和 `vals`，再將兩者傳送回主要的 JS 線程。當它接收到訊息時，首先是重新初始化這三個變數：

```js
let sheet, errs, vals;
self.onmessage = ({data})=>{
  [sheet, errs, vals] = [ data, {}, {} ];
```

為了將座標轉成全域變數，我們用 `for…in` 迴圈，取出 `sheet` 裡面的每個屬性名稱：

```js
  for (const coord in sheet) {
```

ES6 提供了 `const` 和 `let` 關鍵字，來宣告屬於區塊範圍（block scope）的常數與變數。上述的 `const coord`，可以讓迴圈裡定義的函式包入該次迭代裡 `coord` 的實際數值。

如果用舊版 JS 所提供的 `var coord` 來宣告，則變數會屬於函式範圍（function scope），這樣每次迭代時定義的函式，最終都會指向同一個 `coord` 變數。

習慣上，公式裡的變數名稱大小寫視為相同，並且可以加上 `$` 前綴。因為 JS 變數有區分大小寫，所以我們用 `for…of` 循環來取得同一個座標的四種變數名稱表示法：

```js
    // Four variable names pointing to the same coordinate: A1, a1, $A1, $a1
    [ '', '$' ].map( p => [ coord, coord.toLowerCase() ].map(c => {
      const name = p+c;
```

此處用到了箭號函式的簡約語法，用 `p => ...` 來表示 `(p) => { ... }`。

對於每一個變數名稱，例如 `A1` 和 `$a1`，我們在 `self` 上定義其[屬性存取式](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty)，這樣當算式中提到這些變數時，都會自動計算 `vals["A1"]` 的值：

```js
      // Worker is reused across calculations, so only define each variable once
      if ((Object.getOwnPropertyDescriptor( self, name ) || {}).get) { return; }

      // Define self['A1'], which is the same thing as the global variable A1
      Object.defineProperty( self, name, { get() {
```

此處的 `{ get() { … } }` 語法是 `{ get: ()=>{ … } }` 的縮寫。由於我們只定義 `get`，但沒有定義 `set`，這些變數就此成為唯讀，從而免於被使用者鍵入的公式所更改。

`get` 存取函式會先檢查 `vals[coord]`，如果已經計算完畢，則直接將它傳回。

```js
        if (coord in vals) { return vals[coord]; }
```

不然的話，我們就需要從 `sheet[coord]` 計算出 `vals[coord]` 。

首先我們將後者設定為 `NaN`，讓自我指涉的公式（例如將 **A1** 設為 `=A1`）計算出 `NaN`，而不是無限循環：

```js
        vals[coord] = NaN;
```

然後我們檢查 `sheet[coord]` 是不是數字，方法是先用前綴 `+` 將它轉換成數字、把數字寫進 `x`，再將它的字串表現方式與原先的字串進行比較。如有不同，代表它不是數字，那麼我們再將 `x` 設成原本的字符串：

```js
        // Turn numeric strings into numbers, so =A1+C1 works when both are numbers
        let x = +sheet[coord];
        if (sheet[coord] !== x.toString()) { x = sheet[coord]; }
```

如果 `x` 以 `=` 開頭，那它就是公式儲存格。我們使用 `eval.call()` 來運算 `=` 之後的公式部分。第一個引數 `null` 會讓 `eval` 在全域範圍中運行，不讓詞彙範圍（lexical scope）裡的變數（如 `x` 和 `sheet`）影響運算結果：

```js
        // Evaluate formula cells that begin with =
        try { vals[coord] = (('=' === x[0]) ? eval.call( null, x.slice( 1 ) ) : x);
```

如果運算成功，結果會儲存到 `vals[coord]` 裡。對於非公式的儲存格來說，`vals[coord]` 的數值就是 `x`，它可以是數字或字串。

如果 `eval` 發生錯誤， `catch` 區塊會檢查錯誤原因，看看是否因為公式引用了尚未在 `self` 定義的空白儲存格：

```js
        } catch (e) {
          const match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec( e );
          if (match && !( match[0] in self )) {
```

在這種情況下，我們把缺失的儲存格預設成 `0`、清除 `vals[coord]`，並使用 `self[coord]` 來重新執行當前運算：

```js
            // The formula refers to a uninitialized cell; set it to 0 and retry
            self[match[0]] = 0;
            delete vals[coord];
            return self[coord];
          }
```

如果使用者稍後在 `sheet[coord]` 中給出缺失儲存格的內容，那麼 `Object.defineProperty` 會接手，蓋過臨時給的數值。

其他類型的錯誤會寫進 `errs[coord]` 裡：

```js
          // Otherwise, stringify the caught exception in the errs object
          errs[coord] = e.toString();
        }
```

在發生錯誤時，因為賦值操作沒有完成，所以 `vals[coord]` 的數值仍然會是 `NaN`。

最後，`get` 函式傳回 `vals[coord]` 裡計算出的數值，它必須是數字、布林值或字串：

```js
        // Turn vals[coord] into a string if it's not a number or boolean
        switch (typeof vals[coord]) { case 'function': case 'object': vals[coord]+=''; }
        return vals[coord];
      } } );
    }));
  }
```

為每個座標都定義好存取函式之後，背景工作程式再次針對每個座標執行 `self[coord]` 來觸發存取函式，最後將得出的 `errs` 和 `vals` 傳送回主要 JS 線程：

```js
  // For each coordinate in the sheet, call the property getter defined above
  for (const coord in sheet) { self[coord]; }
  return [ errs, vals ];
}
```

### CSS

**styles.css** 檔案只包含少數幾個選擇器的展示樣式。首先我們將表格的所有框線合併在一起，相鄰儲存格之間不留空白：

```css
table { border-collapse: collapse; }
```

標題格和資料格的框線樣式相同，但標題格的背景是淺灰色，資料格是預設的白色，公式儲存格的背景則是淡藍色的：

```
th, td { border: 1px solid #ccc; }
th { background: #ddd; }
td.formula { background: #eef; }
```

在儲存格裡，計算值顯示的寬度是固定的。我們為空白儲存格設定最小高度，並在過長的行尾用省略號標示：

```css
td div { text-align: right; width: 120px; min-height: 1.2em;
         overflow: hidden; text-overflow: ellipsis; }
```

文字的對齊方式和樣式由數值的類型決定，也就是 `text` 和 `error` 這兩個類別選取器：

```css
div.text { text-align: left; }
div.error { text-align: center; color: #800; font-size: 90%; border: solid 1px #800 }
```

至於給使用者編輯的 `input` 輸入框，我們使用絕對定位（absolute position）將它浮在儲存格上，並使其變成透明，讓底下的儲存格數值 `div` 顯示出來：

```css
input { position: absolute; border: 0; padding: 0;
        width: 120px; height: 1.3em; font-size: 100%;
        color: transparent; background: transparent; }
```

當使用者將選取焦點放在某個輸入框上，它就出現在前景：

```css
input:focus { color: #111; background: #efe; }
```

此外，選取輸入框底下的 `div` 會被壓成單行，讓它完全被輸入框覆蓋：

```css
input:focus + div { white-space: nowrap; }
```

## 總結

在這本「500 行以下」的合輯裡，這份用 99 行程式寫成的試算表，只是非常簡約的範例。讀者不妨隨意實驗，將它往任何想得到的方向擴充。

這裡有一些想法，每項都可以在剩餘的 401 行空間裡完成：

* 用 [ShareJS](http://sharejs.org/)、 [AngularFire](http://angularfire.com) 或 [GoAngular](http://goangular.org/) 做出多人共筆的線上編輯器。
* 用 [angular-marked](http://ngmodules.org/modules/angular-marked) 為文字格提供 Markdown 語法。
* 參考 [OpenFormula 標準](https://en.wikipedia.org/wiki/OpenFormula)，實作一些簡單的公式函數（例如 `SUM`、`TRIM` 等）。
* 透過 [SheetJS](http://sheetjs.com/)  來支援常見的試算表格式（CSV、 SpreadsheetML 等）。
* 匯入和匯出線上的試算表服務，例如 Google 試算表和 [EtherCalc](http://ethercalc.net/)。

### 備註: 關於 JS 版本

本章旨在示範 ES6 的新概念，因此使用 [Traceur 編譯器](https://github.com/google/traceur-compiler)將源碼翻譯成 ES5，以在 2015 年之前的瀏覽器上運行。

如果你比較喜歡直接使用 2010 年版的 JS，[as-javascript-1.8.5](https://audreyt.github.io/500lines/spreadsheet/as-javascript-1.8.5/) 目錄下有以 ES5 風格寫成的 **main.js** 和 **worker.js**；它的[源碼](https://github.com/audreyt/500lines/tree/master/spreadsheet/as-javascript-1.8.5)行數和 ES6 版本相同，可以交互對照。

如果你想要更簡潔的語法，[as-livescript-1.3.0](https://audreyt.github.io/500lines/spreadsheet/as-livescript-1.3.0/) 目錄使用 [LiveScript](http://livescript.net/) 寫成 **main.ls** 和 **worker.ls**；它的[源碼](https://github.com/audreyt/500lines/tree/master/spreadsheet/as-livescript-1.3.0)比 JS 版本少了 20 行。

同樣採用 LiveScript 語法，[as-react-livescript](https://audreyt.github.io/500lines/spreadsheet/as-react-livescript/) 目錄使用 [ReactJS](https://facebook.github.io/react/) 框架寫成；它的[源碼](https://github.com/audreyt/500lines/tree/master/spreadsheet/as-react-livescript)比 AngularJS 版本多了 10 行，但運行速度快了許多。

如果你有興趣翻譯這幾段程式到其他 JS 語言，歡迎發送[合併請求](https://github.com/audreyt/500lines/pulls)給我！
