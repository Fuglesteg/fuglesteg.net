---
name: Police certificate system
technologies:
  - vuejs
  - vuetify
  - javascript
  - csharp
  - azure
  - dotnetcore
  - microsoftsqlserver
type: work
date: 01.05.2024
---

# Police certificate system for sports clubs

> Service for the application and registration of police certificates 

The Norwegian Olympic and Paralympic Committee and Confederation of Sports (in norwegian, "Norges Idrettsforbund" or simply NIF) is the main organisation for sports in Norway. They organize volunteer and professional sports and offer a wide area of services to sports clubs all across Norway. These services include "Min Idrett", "My Sport(s)" which is an online portal for athletes and clubs to organize their sports, trainings, and teams.

As a consultant from Evidi I was brought on to develop a section of "Min Idrett" to handle the application process and registration of police certificates. Trainers for children or people with disabilities require a police certificate in Norway. It is is the club's responsibility to make sure that all of their trainers have a certificate. They do this by assigning someone responsible for police certificates who the trainers will show their certificates to, the person then registers that they have shown the certificate and approves them for the role of trainer.

This new system was meant to digitise and simplify this process. In the physical process the trainers had to meet up physically to show their police certificate to the club. This was because the certificates are a major privacy concern and the clubs are therefore not allowed to keep them or ask someone to leave or send the certificates. With the new process we are similarly not allowed to store any data about the certificate itself.

In Norway we have a system of digital mailboxes. These are a safe digital way to be sent private mail. If a person has a digital mailbox then public institutions like the police will prefer to send documents digitally rather than physically. The biggest of these mailboxes is Digipost, which is run by the national mail service (Posten). For this project we collaborated with Digipost to provide a feature for the user where they can choose to share documents from their mailboxes with an organisation (the club). This sharing can also be withdrawn by the user at any point. Our service then only needs a share permission from the user and we can use the document straight from Digipost to show to the club, all without the need to store any info about the certificate.

I was brought on early in the design process and so I had a lot of influence on the final designs and user experience of the service. Most frontend code is written by me and a huge chunk of the backend code, in particular the handling of state and state changes of the police certificate applications as they are sent between the user and the club, along with a way to fetch a timeline of events for the application that can be visualised to the user to illustrate the whole application process. The interface turned out very streamlined for the trainers and the clubs. We received a lot of positive feedback from our users that the new solution revolutionised how they could manage and process police certificates and greatly reduced the time the volunteers would have to spend on this process.

The solution was nominated for the digitalisation award in Norway: [Digitaliseringsprisen 2025](https://www.idrettsforbundet.no/digital/nyheter/digitaliseringsprisen-2025/)
