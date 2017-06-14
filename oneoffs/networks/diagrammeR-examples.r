library("DiagrammeR")


# linxe: 
#   http://www.di.fc.ul.pt/~jpn/r/GraphicalTools/diagrammeR.html

grViz("
  digraph boxes_and_circles {
      
      # a 'graph' statement
      graph [overlap = true, fontsize = 10]
      
      # several 'node' statements
      node [shape = box,
      fontname = Helvetica]
      A; B; C; D; E; F
      
      node [shape = circle,
      fixedsize = true,
      width = 0.9] // sets as circles
      2; 3; 4; 5; 6; 7
      
      node [shape = oval,
      fixedsize = true,
      peripheries = 2,
      width = 0.9,
      fontname = 'Courier New',
      style=filled,
      fillcolor = red]
      8
      
      node [label =<<table border='0' cellborder='1' cellpadding='3' bgcolor='white'>
      <tr><td bgcolor='black' align='center' colspan='2'><font color='white'>header</font> </td></tr>
      <tr><td align='left' port='r2'>&#40;2&#41; &#188; &bull;</td>
      <td bgcolor='grey' align='right'>$</td>
      </tr>
      </table>>
      ] // sets as circles
      1
      
      # several 'edge' statements
      A->1 B->2 B->3 B->4 C->A
      1->D E->A 2->4 1->5 1->F
      E->6 [label='x &#x2208; 3']
      4->6 [label='&#946;&#x2082;=0.85']
      5->7 [penwidth = 5]
      6->7 [style=dotted,label='dotted']
      3->8 [headport = 'e', tailport = 'e', color=blue]
      }
      ")





# workflow of a post graph
DiagrammeR("
           graph TD;
           hw{htmlwidget} -->experiments;
           hw -->content;
           experiments -->RMarkdown
           content -->RMarkdown
           RMarkdown -->|R/knitr|Markdown;
           Markdown -->|Pandoc|HTML;
           HTML -->|git push| Github;
           HTML -->|copy/paste| Squarespace ;
           ")


DiagrammeR("
sequenceDiagram;
           customer->>ticket seller: ask ticket;
           ticket seller->>database: seats;
           alt tickets available
           database->>ticket seller: ok;
           ticket seller->>customer: confirm;
           customer->>ticket seller: ok;
           ticket seller->>database: book a seat;
           ticket seller->>printer: print ticket;
           else sold out
           database->>ticket seller: none left;
           ticket seller->>customer:  sorry;
           end
           ")


DiagrammeR("graph LR;A(Rounded)-->B[Squared];B-->C{A Decision};
 C-->D[Square One];C-->E[Square Two];
 style A fill:#E5E25F;  style B fill:#87AB51; style C fill:#3C8937;
 style D fill:#23772C;  style E fill:#B6E6E6;"
)



data(mtcars)
connections <- sapply(
  1:ncol(mtcars)
  ,function(i) {
    paste0(
      i
      ,"(",colnames(mtcars)[i],")---"
      ,i,"-stats("
      ,paste0(
        names(summary(mtcars[,i]))
        ,": "
        ,unname(summary(mtcars[,i]))
        ,collapse="<br/>"
      )
      ,")"
    )
  }
)

DiagrammeR(
  paste0(
    "graph TD;", "\n",
    paste(connections, collapse = "\n"),"\n",
    "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
    "class ", paste0(1:length(connections), collapse = ","), " column;"
  )
)


