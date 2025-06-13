// ==UserScript==
// @name         Org-protocol Capture Current Page on Alt+l (Template 'l')
// @namespace    http://tampermonkey.net/
// @version      0.5
// @description  Capture the current page's URL and title to Org-mode via org-protocol when Alt+l is pressed, using template 'l'.
// @author       yhiraki
// @match        *://*/*
// @grant        GM_openInTab
// @run-at       document-idle
// ==/UserScript==

(function () {
  "use strict";

  function capturePageWithTemplateL() {
    const url = encodeURIComponent(window.location.href);
    const title = encodeURIComponent(document.title);

    const templateKey = "l";
    const protocolUrl = `org-protocol://capture?url=${url}&title=${title}&template=${templateKey}`;

    location.href = protocolUrl;

    console.log(`Org-protocol URL opened: ${protocolUrl}`);
  }

  // キーボードイベントリスナーを設定
  document.addEventListener("keydown", function (e) {
    if (e.altKey && (e.key === "l" || e.key === "L" || e.code === "KeyL")) {
      e.preventDefault();
      capturePageWithTemplateL();
    }
  });
})();
