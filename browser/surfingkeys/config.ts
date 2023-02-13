// import createAPI from "./Surfingkeys/src/content_scripts/common/api.js";
// const api = createAPI();
// let settings: any;

// Prevent automatic next/previous page loads
settings.smartPageBoundary = false;

// click `save` button to make above settings to take effect.
// set theme
settings.theme = "\
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

api.map("<Ctrl-i>", "D");
api.map("<Ctrl-o>", "S");
api.map("d", "x");
api.map("t", "T");
api.map("u", "X");

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

api.unmap("D");
api.unmap("S");
api.unmap("T");
api.unmap("U");
api.unmap("X");
api.unmap("e");
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
    api.tabOpenLink(text);
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
      break;
    case "o":
      api.Clipboard.write(`[[${url}][${title}]]`);
      break;
  }
});

// yf | Amazon books
api.mapkey(
  "yf",
  "Copy link Amazon: [o]rg, [m]arkdown",
  (key) => {
    const { url, title } = parsePageCurent();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${title}](${url})`);
        break;
      case "o":
        const price = (
          document.getElementById("price") ||
          document.getElementById("kindle-price")
        )?.textContent
          .trim()
          .replace("￥", "")
          .replace(",", "");
        const name = document.getElementById("productTitle")?.textContent;
        const author = Array.from(
          document.querySelectorAll(".author .a-link-normal"),
        ).map(
          (v) => v?.textContent,
        );
        const isbn = document.querySelector("[itemprop=isbn]")?.textContent;
        const date = new Date().toISOString().slice(0, 10);
        api.Clipboard.write(`${name}
:PROPERTIES:
:price: ${price}
:name: ${name}
:author: ${author}
:isbn: ${isbn}
:added_at: <${date}>
:bought_at:
:read_at:
:store_name: amazon
:store_url: ${url}
:ebook: t
:ebook_url:
:END:
`);
        break;
    }
  },
  { domain: /amazon\.co\.jp/ },
);

// yf | Oreilly books
api.mapkey(
  "yf",
  "Copy link Oreilly: [o]rg, [m]arkdown",
  (key) => {
    const { url, title } = parsePageCurent();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${title}](${url})`);
        break;
      case "o":
        const price = (() => {
          const options = document.querySelectorAll(".buying-options > div");
          return options[0].childNodes[3]?.textContent.trim().replace(
            /[円,]/g,
            "",
          );
        })();
        const name = document.querySelector("[itemprop=name]").textContent;
        const author = document.querySelector("[itemprop=author]").textContent;
        const isbn = document.querySelector("[itemprop=isbn]").textContent;
        const date = new Date().toISOString().slice(0, 10);
        api.Clipboard.write(`${name}
:PROPERTIES:
:price: ${price}
:name: ${name}
:author: ${author}
:isbn: ${isbn}
:added_at: <${date}>
:bought_at:
:read_at:
:store_name: oreilly
:store_url: ${url}
:ebook: t
:ebook_url:
:END:
`);
        break;
    }
  },
  { domain: /oreilly\.co\.jp\/\/?books\/\d+/ },
);

// yf | Lambdanote books
api.mapkey(
  "yf",
  "Copy link Lambdanote: [o]rg, [m]arkdown",
  (key) => {
    const { url, title } = parsePageCurent();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${title}](${url})`);
        break;
      case "o":
        const price = Number(
          document.querySelector('meta[property="og:price:amount"]')?.content
            .replace(",", ""),
        );
        const price_tax = Math.round(price * 1.1 * 100000) / 100000;
        const name = document.querySelector('meta[property="og:title"]')
          ?.content;
        const author = "";
        const isbn = "";
        const date = new Date().toISOString().slice(0, 10);
        api.Clipboard.write(`${name}
:PROPERTIES:
:price: ${price_tax}
:name: ${name}
:author: ${author}
:isbn: ${isbn}
:added_at: <${date}>
:bought_at:
:read_at:
:store_name: lambdanote
:store_url: ${url}
:ebook: t
:ebook_url:
:END:
`);
        break;
    }
  },
  { domain: /www\.lambdanote\.com\/products/ },
);

// yf | Gihyo books
api.mapkey(
  "yf",
  "Copy link Gihyo: [o]rg, [m]arkdown",
  (key) => {
    const { url, title } = parsePageCurent();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${title}](${url})`);
        break;
      case "o":
        const price = document
          .querySelector(".buy")
          .firstChild.textContent.trim()
          .replace(/[円,]/g, "");
        const name = document.getElementById("bookTitle").textContent;
        const author = document
          .querySelector("[itemprop=author]")
          .textContent.replace(/\u3000著$/, "");
        const isbn = "";
        const date = new Date().toISOString().slice(0, 10);
        api.Clipboard.write(`${name}
:PROPERTIES:
:price: ${price}
:name: ${name}
:author: ${author}
:isbn: ${isbn}
:added_at: <${date}>
:bought_at:
:read_at:
:store_name: gihyo
:store_url: ${url}
:ebook: t
:ebook_url:
:END:
`);
        break;
    }
  },
  { domain: /gihyo\.jp\/dp\/ebook\// },
);

// yf | SEShop books
api.mapkey(
  "yf",
  "Copy link SEShop: [o]rg, [m]arkdown",
  (key) => {
    const { url, title } = parsePageCurent();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${title}](${url})`);
        break;
      case "o":
        const price = document.getElementsByName("cxenseparse:sho-price")[0]
          ?.content;
        const name = document.getElementsByName(
          "cxenseparse:sho-product-name",
        )[0]?.content;
        const author = Array.from(
          document.getElementsByName("cxenseparse:sho-author"),
        ).map(
          (v) => v?.content,
        );
        const isbn = document.getElementsByName("cxenseparse:sho-isbn")[0]
          ?.content;
        const date = new Date().toISOString().slice(0, 10);
        api.Clipboard.write(`${name}
:PROPERTIES:
:price: ${price}
:name: ${name}
:author: ${author}
:isbn: ${isbn}
:added_at: <${date}>
:bought_at:
:read_at:
:store_name: seshop
:store_url: ${url}
:ebook: t
:ebook_url:
:END:
`);
        break;
    }
  },
  { domain: /seshop.com\/product\// },
);

// https://raw.githubusercontent.com/yhiraki/dotfiles/master/browser/surfingkeys/config.ts

const parsePageBitbucket = () => {
  return {
    issueTitle: document.querySelectorAll("#pull-request-details header div")[2]
      .textContent,
  };
};

const parseURLBitbucket = (rawUrl = location.href) => {
  const url = new URL(rawUrl);
  return {
    repoName: url.pathname.split("/").slice(1, 3).join("/"),
    issueNo: Number(url.pathname.split("/")[4]),
  };
};

// yf | [Bitbuket PR] copy link
api.mapkey(
  "yf",
  "[Bitbuket PR] copy link: [o]rg, [m]arkdown",
  (key) => {
    const { url } = parsePageCurent();
    const { issueTitle } = parsePageBitbucket();
    const { issueNo, repoName } = parseURLBitbucket();
    switch (key) {
      case "m":
        api.Clipboard.write(`${repoName} [#${issueNo}](${url}) ${issueTitle}`);
        break;
      case "o":
        api.Clipboard.write(
          `${repoName} [[${url}][#${issueNo}]] ${issueTitle}`,
        );
        break;
    }
  },
  { domain: /bitbucket\.org\/.*\/pull-requests\/\d+/ },
);

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

// yf | [GitHub PR/Issues] Copy link
api.mapkey(
  "yf",
  "[GitHub PR/Issues] Copy link: [o]rg, [m]arkdown",
  (key) => {
    const { url } = parsePageCurent();
    const { issueTitle } = parsePageGithub();
    const { issueNo, repoName } = parseURLGithub();
    switch (key) {
      case "m":
        api.Clipboard.write(`${repoName} [#${issueNo}](${url}) ${issueTitle}`);
        break;
      case "o":
        api.Clipboard.write(
          `${repoName} [[${url}][#${issueNo}]] ${issueTitle}`,
        );
        break;
    }
  },
  { domain: /github\.com\/.*\/(pull|issues)\/\d+/ },
);

// yF | [GitHub PR/Issues] Copy links
api.mapkey(
  "yF",
  "[GitHub PR/Issues] Copy links: [o]rg, [m]arkdown",
  (key) => {
    const links = (() => {
      const links = Array.from(document.querySelectorAll("a")).filter((v) =>
        /\/(pull|issues)\/\d+$/.test(v.href)
      );
      const linkMap = new Map();
      for (const l of links) {
        // only use first link of same hrefs
        if (!linkMap.get(l.href)) linkMap.set(l.href, l);
      }
      return Array.from(linkMap.values()).map((v) => {
        const { issueNo, repoName } = parseURLGithub(v.href);
        switch (key) {
          case "m":
            return `${repoName} [#${issueNo}](${v.href}) ${v.textContent}`;
          case "o":
            return `${repoName} [[${v.href}][#${issueNo}]] ${v.textContent}`;
        }
      });
    })();
    api.Clipboard.write(links.join("\n"));
  },
  { domain: /github\.com\/([^\/]+\/.*\/)*(pulls|issues)\// },
);

const parsePageBacklogTicket = () => {
  return {
    ticketKey: document.querySelector(".ticket__key-number")?.textContent,
    ticketTitle: document.querySelector(".ticket__title")?.textContent,
    summaryMarkdown: document.querySelector(".ticket__collapsed-summary")
      ?.textContent,
  };
};

// yf | [Backlog] Copy link
api.mapkey(
  "yf",
  "[Backlog] Copy link: [o]rg, [m]arkdown",
  (key) => {
    const { url } = parsePageCurent();
    const { ticketKey, ticketTitle } = parsePageBacklogTicket();
    switch (key) {
      case "m":
        api.Clipboard.write(`[${ticketKey}](${url}) ${ticketTitle}`);
        break;
      case "o":
        api.Clipboard.write(`[[${url}][${ticketKey}]] ${ticketTitle}`);
        break;
    }
  },
  { domain: /backlog\.jp\/view/ },
);

// yF | [Backlog] copy links
api.mapkey(
  "yF",
  "[Backlog] Copy links: [o]rg, [m]arkdown",
  (key) => {
    const links = Array.from(document.querySelectorAll("#issues-table tr"))
      .slice(1)
      .map((v) => {
        const a = v.querySelector("a");
        const status = v.querySelector(".cell-status").textContent;
        const ticketKey = v.querySelector(".cell-key").textContent;
        const ticketTitle = v.querySelector(".cell-summary").textContent;
        switch (key) {
          case "m":
            return `${status} [${ticketKey}](${a.href}) ${ticketTitle}`;
          case "o":
            return `${status} [[${a.href}][${ticketKey}]] ${ticketTitle}`;
        }
      });
    api.Clipboard.write(links.join("\n"));
  },
  { domain: /backlog\.jp\/find/ },
);

// yb | [Backlog] Copy body
api.mapkey(
  "yb",
  "[Backlog] Copy body: [m]arkdown",
  (key) => {
    switch (key) {
      case "m":
        const { summaryMarkdown } = parsePageBacklogTicket();
        api.Clipboard.write(summaryMarkdown);
        break;
    }
  },
  { domain: /backlog\.jp\/view/ },
);

function parsePageBacklogWiki() {
  const d = {
    WikiId: document.querySelector(".ticket__key-number")?.textContent,
    bodyHTML: document.querySelector(".markdown-body")?.outerHTML,
  };
  return d;
}

// yb | [Backlog] Copy body
api.mapkey(
  "yb",
  "[Backlog] Copy body: [h]TML",
  (key) => {
    switch (key) {
      case "h":
        const { bodyHTML } = parsePageBacklogWiki();
        api.Clipboard.write(bodyHTML);
        break;
    }
  },
  { domain: /backlog\.jp\/wiki/ },
);

api.unmapAllExcept(
  ["d", "E", "R", "T", "b", "cf", "f", "r", "t"],
  /(mail|drive|docs)\.google\.com/,
);
