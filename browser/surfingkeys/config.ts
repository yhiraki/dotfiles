// import createAPI from "./Surfingkeys/src/content_scripts/common/api.js";
// const api = createAPI();
// let settings: any;

settings.blacklistPattern =
  /.*mail.google.com.*|.*inbox.google.com.*|trello.com|duolingo.com|youtube.com|udemy.com|localhost/i;

// Prevent automatic next/previous page loads
settings.smartPageBoundary = false;

// click `save` button to make above settings to take effect.
// set theme
settings.theme =
  "\
.sk_theme { \
    background: #fff; \
    color: #000; \
} \
.sk_theme tbody { \
    color: #000; \
} \
.sk_theme input { \
    color: #000; \
} \
.sk_theme .url { \
    color: #555; \
} \
.sk_theme .annotation { \
    color: #555; \
} \
.sk_theme .focused { \
    background: #f0f0f0; \
}";

settings.smoothScroll = false;

// Always use omnibar to select tabs
settings.tabsThreshold = 0;

api.map("d", "x");
api.map("u", "X");
api.map("<Ctrl-i>", "D");
api.map("<Ctrl-o>", "S");

api.iunmap(":");
api.iunmap("<Alt-b>");
api.iunmap("<Alt-d>");
api.iunmap("<Alt-f>");
api.iunmap("<Alt-w>");
api.iunmap("<Ctrl-'>");
api.iunmap("<Ctrl-a>");
api.iunmap("<Ctrl-b>");
api.iunmap("<Ctrl-e>");
api.iunmap("<Ctrl-f>");
api.iunmap("<Ctrl-u>");
api.iunmap("<Esc>");

api.unmap("e");
api.unmap("U");
api.unmap("D");
api.unmap("S");
api.unmap("X");
api.unmap("ob");
api.unmap("oe");
api.unmap("os");
api.unmap("ow");
api.unmap("oy");
api.unmap("x");
api.unmap("yf");

const getURLCurrent = () => {
  const url = location.href;

  if (/amazon\.co\.jp/.test(url)) {
    const asin = document.querySelector("#ASIN")?.value;
    if (asin !== undefined) {
      return `${location.origin}/dp/${asin}`;
    }
  }

  if (/atlassian\.net/.test(url)) {
    const shorten = document.querySelector(
      'input[value^="https://"][value*="/cp/"]'
    )?.value;
    if (shorten !== undefined) {
      return shorten;
    }
  }

  return url;
};

api.mapkey("yy", "Copy current page's URL", () => {
  api.Clipboard.write(getURLCurrent());
});

const readClipboardAndGo = (cb) => {
  api.Clipboard.read(function (res) {
    const text = res.data;
    if (text.startsWith("http://") || text.startsWith("https://")) {
      cb(text);
      return;
    }
    cb(`https://www.google.com/search?q=${text}`);
  });
};

api.mapkey("p", "Paste a link or search", () => {
  readClipboardAndGo((text) => {
    location.href = text;
  });
});

api.mapkey("P", "Paste a link or search (NewTab)", () => {
  readClipboardAndGo((text) => {
    open(text);
  });
});

const parsePageCurent = () => {
  return {
    url: getURLCurrent(),
    title: document.title,
  };
};

api.mapkey("yf", "Copy link: [o]rg, [m]arkdown", (key) => {
  const { url, title } = parsePageCurent();
  switch (key) {
    case "m":
      api.Clipboard.write(`[${title}](${url})`);
    case "o":
      api.Clipboard.write(`[[${url}][${title}]]`);
  }
});

const parsePageGithub = () => {
  return {
    issueTitle: document.querySelector(".markdown-title")?.textContent,
  };
};

const parseURLGithub = (rawUrl = location.href) => {
  const url = new URL(rawUrl);
  return {
    repoName: url.pathname.split("/").slice(1, 3).join("/"),
    issueNo: Number(url.pathname.split("/")[4]),
  };
};

api.mapkey(
  "yf",
  "[GitHub PR/Issues] copy link: [o]rg, [m]arkdown",
  (key) => {
    const { url } = parsePageCurent();
    const { issueTitle } = parsePageGithub();
    const { issueNo, repoName } = parseURLGithub();
    switch (key) {
      case "m":
        api.Clipboard.write(`${repoName} [#${issueNo}](${url}) ${issueTitle}`);
      case "o":
        api.Clipboard.write(
          `${repoName} [[${url}][#${issueNo}]] ${issueTitle}`
        );
    }
  },
  { domain: /github\.com\/.*\/(pull|issues)\/\d+/ }
);

api.mapkey(
  "yF",
  "[GitHub PR/Issues] copy links [o]rg, [m]arkdown",
  (key) => {
    const links = Array.from(document.querySelectorAll("a"))
      .filter((v) => /\/(pull|issues)\/\d+$/.test(v.href))
      .sort((a, b) => Number(a.href > b.href))
      .filter((v, i, arr) => i > 0 && v.href !== arr[i - 1].href) // unique
      .map((v) => {
        const { issueNo, repoName } = parseURLGithub(v.href);
        switch (key) {
          case "m":
            return `${repoName} [#${issueNo}](${v.href}) ${v.textContent}`;
          case "o":
            return `${repoName} [[${v.href}][#${issueNo}]] ${v.textContent}`;
        }
      });
    api.Clipboard.write(links.join("\n"));
  },
  { domain: /github\.com\/(pulls|issues)$/ }
);

const parsePageBacklogTicket = () => {
  return {
    ticketKey: document.querySelector(".ticket__key-number")?.textContent,
    ticketTitle: document.querySelector(".ticket__title")?.textContent,
    summaryMarkdown: document.querySelector(".ticket__collapsed-summary")
      ?.textContent,
  };
};

api.mapkey(
  "yf",
  "[Backlog] copy link [o]rg, [m]arkdown",
  (key) => {
    const { url } = parsePageCurent();
    const { ticketKey, ticketTitle } = parsePageBacklogTicket();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${ticketKey}](${url}) ${ticketTitle}`);
      case "o":
        api.Clipboard.write(`[[${url}][${ticketKey}]] ${ticketTitle}`);
    }
  },
  { domain: /backlog\.jp\/view/ }
);

api.mapkey(
  "yb",
  "[Backlog] Copy body [m]arkdown",
  (key) => {
    switch (key) {
      case "m":
        const { summaryMarkdown } = parsePageBacklogTicket();
        api.Clipboard.write(summaryMarkdown);
    }
  },
  { domain: /backlog\.jp\/view/ }
);

function parsePageBacklogWiki() {
  const d = {
    WikiId: document.querySelector(".ticket__key-number")?.textContent,
    bodyHTML: document.querySelector(".markdown-body")?.outerHTML,
  };
  return d;
}

api.mapkey(
  "yb",
  "[Backlog] Copy body [h]TML",
  (key) => {
    switch (key) {
      case "h":
        const { bodyHTML } = parsePageBacklogWiki();
        api.Clipboard.write(bodyHTML);
    }
  },
  { domain: /backlog\.jp\/wiki/ }
);

api.unmapAllExcept(
  ["E", "R", "T", "b", "cf", "f", "r", "t"],
  /mail.google.com/
);
