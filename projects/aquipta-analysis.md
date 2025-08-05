---
name: Data gathering for migraine research
technologies:
  - lisp
  - ollama
  - selenium
date: 10.01.2025
---
    
# Data gathering for medical research

> Extracting metrics with LLMs and browser automation

I was given an opportunity to help out on a research project for a trial of a new medicine to reduce migraines. Datasets were gathered by the doctor giving the treatment in their journals. This was however a massive problem as there were thousands of journals across over 100 patients. The data in the journals were not formatted or written in a predictable way. This means that it would be extremely hard, even impossible to extract the data using traditional programs. As the data was written in plain Norwegian. The solution was to extract the data using an LLM (deespeek-r1) to extract data from individual journals and compiling this data using more traditional techniques.

The first challenge was simply extracting the journals themselves from the web interface that the doctor had written the journals. There was no public API or way to extract all the documents. Most of the site was also using SSR so extracting the journals by reverse engineering some kind of API would be next to impossible. Instead I opted to use Selenium, a tool for automating the web browser with an API. I used a client library for Common Lisp to interactively program the browser from the REPL, using this way of interactive development I was very easily able to create a routine that could take a list of patient identifiers, search them up in the interface and extract all the text from the journals.

The next and most difficult task was extracting the data we are after from the journals. The choice of using an LLM was a natural one, because the data was embedded in natural language, following no structure there was really no other way of reliably extracting it. Because the journals contain sensitive information sending the data to any AI services was out of the question. Instead I opted to run the LLM locally using ollama. After trying out a few models I got some inconsistent results, I came up with a few ways to mitigate the inconsistency, like rerunning the same prompts and only using the most consistent answer. Most of these mitigations were however not as necessary when I switched models to deepseek-r1, which had just come out as I was working on this project. Using r1 I got way more consistent results and felt much more confident that this solution would be accurate. Because I use my main computer as a gaming rig I have a quite powerful graphics card that was able to run a version of deepseek with 8 billion parameters. Using the local REST based API of ollama through a library cl-ollama I was able to feed data from the journals into the local LLM and ask it questions to extract the necessary data. The common lisp program ended up being quite big, the parameters required for each patient was very sunstantial and would often require comparing values from different journals. There was also a diary format of headache days and their severity. Which was quite consistent, so I wrote a custom parser  for this format to lessen the burden on the LLM and the chance of inaccurate data. The parser had to be very forgiving with missing commas, or full stops instead of commas or misspellings, weird casing etc. etc.

At the end the program had to generate a massive csv file with the results. I also generated a csv file with the journals so that the other volunteers could easily look through and verify the main data. The volunteers were very happy with how easy they could verify the data and there was a very small error rate and missing data.

Because of the massive amount of data and the use of an LLM, gathering the final dataset took a **long** time. The program ran for 48 hours on my single CPU/GPU computer. Because of the interactive nature and IDE tooling available for Common Lisp I could freely inspect the state of the program while it was running and even pause and resume the execution freely.

This project was a massive effort and at times massive headache for me. We had a deadline which kept getting moved up, I was working on this in my free time next to my full time job and we kept getting new requirements. I did learn a lot and got to refine my methods and process working with Common Lisp. Common Lisp was the perfect choice for this project, the interactive nature of the language was great for experimenting and fine-tuning the prompts. The deep interactive nature is also great for fine tuning the prigram while it runs. Because the program required so much time to run I could introspect the program and temporary results to see if it was working correctly.

The research papers for this project are not yet fully released, but the page will be updated once they are.