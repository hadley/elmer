# more realistic wrap examples

    Code
      cat(capture_sink(prefix = "?> ", width = 70, function(sink) {
        sink("It was the best of times, it was the worst of times, it was the age ")
        sink("of wisdom, it was the age of foolishness, it was the epoch of belief")
        sink(", it was the epoch of incredulity, it was the season of Light, it wa")
        sink("s the season of Darkness, it was the spring of hope, it was the wint")
        sink("er of despair.")
      }))
    Output
      ?> It was the best of times, it was the worst of times, it was the 
      ?> age of wisdom, it was the age of foolishness, it was the epoch of 
      ?> belief, it was the epoch of incredulity, it was the season of 
      ?> Light, it was the season of Darkness, it was the spring of hope, 
      ?> it was the winter of despair.

---

    Code
      cat(capture_sink(prefix = "?> ", width = 10000, function(sink) {
        sink("It was the best of times, it was the worst of times, it was the age ")
        sink("of wisdom, it was the age of foolishness, it was the epoch of belief")
        sink(", it was the epoch of incredulity, it was the season of Light, it wa")
        sink("s the season of Darkness, it was the spring of hope, it was the wint")
        sink("er of despair.")
      }))
    Output
      ?> It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair.

---

    Code
      cat(capture_sink(prefix = "", width = 70, function(sink) {
        sink("It was the best of times, it was the worst of times, it was the age ")
        sink("of wisdom, it was the age of foolishness, it was the epoch of belief")
        sink(", it was the epoch of incredulity, it was the season of Light, it wa")
        sink("s the season of Darkness, it was the spring of hope, it was the wint")
        sink("er of despair.")
      }))
    Output
      It was the best of times, it was the worst of times, it was the age 
      of wisdom, it was the age of foolishness, it was the epoch of belief,
      it was the epoch of incredulity, it was the season of Light, it was 
      the season of Darkness, it was the spring of hope, it was the winter 
      of despair.

---

    Code
      cat(capture_sink(prefix = "", width = 10000, function(sink) {
        sink("It was the best of times, it was the worst of times, it was the age ")
        sink("of wisdom, it was the age of foolishness, it was the epoch of belief")
        sink(", it was the epoch of incredulity, it was the season of Light, it wa")
        sink("s the season of Darkness, it was the spring of hope, it was the wint")
        sink("er of despair.")
      }))
    Output
      It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair.

---

    Code
      cat(capture_sink(prefix = "Dickens> ", function(sink) {
        sink("It was the best of times,\nit was the worst of times,\n")
        sink("it was the age ")
        sink("of wisdom, it was the age of ")
        sink("foolishness,\nit was the epoch of belief")
        sink(", it was the epoch of incredulity, ")
        sink(paste(rep_len(" ", 200), collapse = ""))
        sink("it was the season of Light, it wa")
        sink("s the season of Darkness,\n\n\nit was the spring of hope,")
        sink(" it was the wint")
        sink("er of despair.")
      }))
    Output
      Dickens> It was the best of times,
      Dickens> it was the worst of times,
      Dickens> it was the age of wisdom, it was the age of 
      Dickens> foolishness,
      Dickens> it was the epoch of belief, it was the epoch of 
      Dickens> incredulity,
      Dickens> it was the season of Light, it was the season of 
      Dickens> Darkness,
      Dickens> 
      Dickens> 
      Dickens> it was the spring of hope, it was the winter of 
      Dickens> despair.

