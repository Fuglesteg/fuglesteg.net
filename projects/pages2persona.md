---
name: Pages2Persona
technologies:
    - svelte
    - typescript
    - csharp
    - dotnetcore
    - azure
    - cosmosdb
    - azuredevops
---

# Pages2Persona

> Talk to your favorite characters in literature

Pages2Persona is a web app that you can upload books to and then talk to the
characters in that book. A user would upload a book with a character that they
want to talk to. Then they would register the characters in the book that they
wish to talk to. And then they can start a conversation with those characters.
The application would parse and index the books and then store them in a vector
database. When a conversation is started the LLM would get a custom prompt to
behave like the character and exempt their traits. For example you could upload
Moby Dick and talk to Ishmael or Captain Ahab. The LLM would try to mimic the
character's traits which it learns from querying the vector database. The
application would also allow the user to see where the LLM got the information
that it is using. Any time it used information from the vector database, the
user could click hyperlinks to see the original chapters from the book that the
LLM is currently using.

Pages2Persona was a case study project during [Microsoft University](https://www.microsoft.com/nb-no/microsoftuniversity/) a training
program for the newly educated in Norway. During the course I learned about
Azure and .NET, and took two Azure certifications, AZ-200 and AZ-400, the web
app was entirely hosted on Azure and used mostly Microsoft technologies. Azure
AI Services had just been put in pre release and so it inspired us to think of
project ideas that would use these services.

![Architecture Diagram](/public/Pages2Persona.png)
