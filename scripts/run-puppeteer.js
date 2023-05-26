import puppeteer from "puppeteer";

(async () => {
  const browser = await puppeteer.launch({ headless: "new" });

  let page;

  console.log("UI3");
  page = await browser.newPage();
  await page.goto("http://localhost:1234#ui3");
  await page.setViewport({ width: 200, height: 150 });
  await page.screenshot({ path: "./assets/img/ui3.png" });

  console.log("UI2");
  page = await browser.newPage();
  await page.goto("http://localhost:1234#ui2");
  await page.setViewport({ width: 200, height: 150 });
  await page.screenshot({ path: "./assets/img/ui2.png" });

  console.log("UI1");
  page = await browser.newPage();
  await page.goto("http://localhost:1234#ui1");
  await page.setViewport({ width: 200, height: 150 });
  await page.screenshot({ path: "./assets/img/ui1.png" });

  console.log("UI Record");
  page = await browser.newPage();
  await page.goto("http://localhost:1234#ui-record");
  await page.setViewport({ width: 400, height: 450 });
  await page.screenshot({ path: "./assets/img/ui-record.png" });

  console.log("UI Variant");
  page = await browser.newPage();
  await page.goto("http://localhost:1234#ui-variant");
  await page.setViewport({ width: 400, height: 450 });
  await page.screenshot({ path: "./assets/img/ui-variant.png" });

  await browser.close();
})();
