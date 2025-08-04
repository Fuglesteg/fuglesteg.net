---
name: Wordpress archive
technologies:
    - wordpress
    - php
    - javascript
date: 10.04.2022
---

# Wordpress Archive

> An OCR based archival solution implemented in Wordpress

This was a project for a local sports organisation that we collaborated with
during university. The organisation has a long history of more than 60 years
and wanted to digitise their archive of old news clippings, pictures, documents,
etc. This is mostly a volunteer organisation, so our solution had to prioritise
the time of the volunteers. We achieved this by designing a streamlined flow for
the volunteers who would be digitising the archive.

The system integrated with the organisations OneDrive to store all images and
documents. We created a WordPress plugin that functioned as a Single Page
Application implemented in vanilla JavaScript. There the user would pick files
from the organisations OneDrive to create a post. This was usually a collection
of scanned news clippings or images from past events etc. Our system would then
scan the images using an OCR service we had created for the WordPress page. The
OCR endpoints used [Tesseract OCR](https://github.com/tesseract-ocr/tesseract)
to read text in both Norwegian and English from the images. The service would
also extract text from PDFs. We used this text to amplify the ability to search
for documents.

The archive was displayed to the clubs members using a touchscreen stationed at
the organisations club house. The page therefore had to be easy to use with
touch gestures. We included web based touchscreen keyboards that users could
use to input search terms and other inputs. The system also had a powerful
search feature based on the WordPress API.
