import puppeteer from "puppeteer";
import { PuppeteerScreenRecorder } from "puppeteer-screen-recorder";

(async () => {
  const browser = await puppeteer.launch({ headless: "new" });

  let page;
  let recorder;

  console.log("UI1")
  page = await browser.newPage();
  await page.setViewport({ width: 200, height: 150 });
  recorder = new PuppeteerScreenRecorder(page);
  await page.goto("http://localhost:1234#ui1");
  await demoUi1.prepare(page);
  await recorder.start("./assets/gif/ui1.mp4");
  await demoUi1.run(page);
  await recorder.stop();

  console.log("UI2")
  page = await browser.newPage();
  await page.setViewport({ width: 200, height: 150 });
  recorder = new PuppeteerScreenRecorder(page);
  await page.goto("http://localhost:1234#ui2");
  await demoUi2.prepare(page);
  await recorder.start("./assets/gif/ui2.mp4");
  await demoUi2.run(page);
  await recorder.stop();

  console.log("UI3")
  page = await browser.newPage();
  await page.setViewport({ width: 200, height: 150 });
  recorder = new PuppeteerScreenRecorder(page);
  await page.goto("http://localhost:1234#ui3");
  await demoUi3.prepare(page);
  await recorder.start("./assets/gif/ui3.mp4");
  await demoUi3.run(page);
  await recorder.stop();

  console.log("UI Record")
  page = await browser.newPage();
  await page.setViewport({ width: 400, height: 450 });
  recorder = new PuppeteerScreenRecorder(page);
  await page.goto("http://localhost:1234#ui-record");
  await recorder.start("./assets/gif/ui-record.mp4");
  await demoUi1.run(page);
  await demoUi2.run(page);
  await demoUi3.run(page);
  await recorder.stop();

  console.log("UI Variant")
  page = await browser.newPage();
  await page.setViewport({ width: 400, height: 450 });
  recorder = new PuppeteerScreenRecorder(page);
  await page.goto("http://localhost:1234#ui-variant");
  await recorder.start("./assets/gif/ui-variant.mp4");
  await recorder.stop();

  await browser.close();
})();

function delay(time) {
  return new Promise(function (resolve) {
    setTimeout(resolve, time);
  });
}

const demoUi1 = {
  prepare: () => {},
  run: async (page) => {
    const buttonMore = await page.waitForSelector("#ui1 #more");
    const buttonLess = await page.waitForSelector("#ui1 #less");

    await delay(500);
    await buttonMore?.click();
    await delay(500);
    await buttonMore?.click();
    await delay(500);
    await buttonMore?.click();

    await delay(500);
    await buttonLess?.click();
    await delay(500);
    await buttonLess?.click();
    await delay(500);
    await buttonLess?.click();
  },
};

const demoUi2 = {
  prepare: async (page) => {
    const textInput = await page.waitForSelector("#ui2 input");
    textInput?.type("");
  },
  run: async (page) => {
    const textInput = await page.waitForSelector("#ui2 input");

    textInput?.type("H");
    await delay(100);
    textInput?.type("e");
    await delay(100);
    textInput?.type("l");
    await delay(100);
    textInput?.type("l");
    await delay(100);
    textInput?.type("o");
    await delay(100);
    textInput?.type("!");
    await delay(100);
    await page.keyboard.press("Backspace");
    await delay(100);
    await page.keyboard.press("Backspace");
    await delay(100);
    await page.keyboard.press("Backspace");
    await delay(100);
    await page.keyboard.press("Backspace");
    await delay(100);
    await page.keyboard.press("Backspace");
    await delay(100);
    await page.keyboard.press("Backspace");
    await delay(100);
  },
};

const demoUi3 = {
  prepare: async () => {},
  run: async (page) => {
    const input = await page.waitForSelector("#ui3 input");

    await delay(1000);
    input?.click();
    await delay(1000);
    input?.click();
  },
};
