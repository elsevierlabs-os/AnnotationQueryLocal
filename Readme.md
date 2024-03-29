# AnnotationQueryLocal

AnnotationQueryLocal provides a suite of composable functions to query annotations stored as an Array.  While the annotations will typically be generated by popular text analytic tools such as Stanford Core or Genia, the only requirement is the annotations adhere to the AQAnnotation structure.

Unlike [AnnotationQuery](https://github.com/elsevierlabs-os/AnnotationQuery), AnnotationQueryLocal does not use SparkSQL or the parquet annotation files.  Instead, the individual caret delimited annotation files are converted to an Array[AQAnnotation] and used as input with each file (document) queried in isolation.  An Array[AQAnnotation] should only contain annotations for one document.  Similarly,  AnnotationQueryLocal functions defined below should only be applied to Array[AQAnnotation] for the same document.

The signatures for the functions associated with AnnotationQuery and AnnotationQueryLocal are very similar so it should be trivial to migrate code between the two approaches.

#### AQAnnotation

AQAnnotation (defined in com.elsevier.aql.annotations) is the runtime format that is used by AnnotationQueryLocal.  We make reference to a CaretAnnotation format as well.  While this is not an official class, it is the archive serialized format.   This was done to insulate the AnnotationQueryLocal implementation from the archive format providing flexibility for future optimizations.  The AQAnnotation record should never be archived (only the CaretAnnotation records).

CaretAnnotations for the same document are kept in the same file (with docId as the filename). They are also typically separated by annotation set as well. The CaretAnnotation records have the following format.  
```
annotationId ^ annotationSet ^ annotationType ^ startOffset ^ endOffset ^ other
 - annotationId is a unique id (within the annotation file) for this document
 - annotationSet identifies the set for the annotation (such as scnlp for Stanford Core, ge for Genia, etc.)
 - annotationType identifies the type of annotation within the set (such as sentence or text for Stanford Core)
 - startOffset is the starting offset within the file for the document annotated
 - endOffset is the ending offset within the file for the document annotated
 - other are name-value parameters (ampersand delimited)

1^ge^sentence^64261^64302^parm1=value1&parm2=value2
```

At runtime, AnnotationQueryLocal uses the following record structure.  A utility function is provided to transform a CaretAnnotation record into a AQAnnotation record.  The name-value pairs defined in the other column (of CaretAnnotation) will be converted to a Map with the name-value pairs.

```
case class AQAnnotation(docId: String,                                   // Document Id (PII)
                         annotSet: String,                               // Annotation set (such as scnlp, ge)
                         annotType: String,                              // Annotation type (such as text, sentence)
                         startOffset: Long,                              // Starting offset for the annotation
                         endOffset: Long,                                // Ending offset for the annotation
                         annotId: Long,                                  // Annotation Id
                         properties: Option[scala.collection.Map[String,String]] = None)  // Properties
```

#### Utilities

The GetAQAnnotation and GetCaretAnnotation and utility classes have been developed to create an AQAnnotation from the archive format (Caret Annotation) and vise versa.  These are both defined in the com.elsevier.aql.utilities package. When creating the AQAnnotation,  the ampersand separated string of name-value pairs in the CaretAnnotation other field is mapped to a Map in the AQAnnotation record.  To minimize memory consumption and increase performance, you can specify which name-value pairs to include in the Map.  For usage examples, view the GetAQAnnotation and GetCaretAnnotation classes in the test package.

#### AnnotationQuery Functions

The following functions are currently provided by AnnotationQuery. Since functions return a Array[AQAnnotation], it is possible to nest function calls.  For more details on the implementation, view the corresponding class for each function in the com.elsevier.aql.query package.  For usage examples, view the QuerySuite class in the test package.  Each of the functions provided by AnnotationQuery are implemented as a separate Object.  The BinaryOp Object provides the equivalence of a left-semi join with the provided conditions.

**FilterProperty**  -  Provide the ability to filter an Array of AQAnnotations based on the value matching the specified property value in the map.  Returns the filtered Array of AQAnnotation.

**RegexProperty**  -  Provide the ability to filter an Array of AQAnnotations based on the regex applied to the specified property value in the map.  Returns the filtered Array of AQAnnotation.

**FilterSet**  -  Provide the ability to filter an Array of AQAnnotations based on the value in the annotSet field. You can provide a single value or list values to filter on. Returns the filtered Array of AQAnnotation.

**FilterType**  -  Provide the ability to filter an Array of AQAnnotations based on the value in the annotType field. You can provide a single value or list values to filter on. Returns the filtered Array of AQAnnotation.

**Contains**  - Provide the ability to find annotations that contain another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A that contain B. What that means is the start/end offset for an annotation from A must contain the start/end offset from an annotation in B. The start/end offsets are inclusive.  We ultimately return the container annotations (A) that meet this criteria. We also deduplicate the A annotations as there could be many annotations from B that could be contained by an annotation in A but it only makes sense to return the unique container annotations. There is also the option of negating the query (think Not Contains) so that we return only A where it does not contain B.

**ContainedIn**  -  Provide the ability to find annotations that are contained by another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A that are contained in B. What that means is the start/end offset for an annotation from A must be contained by the start/end offset from an annotation in B. The start/end offsets are inclusive.  We ultimately return the contained annotations (A) that meet this criteria. There is also the option of negating the query (think Not Contains) so that we return only A where it is not contained in B.

**ContainedInList**  -  Provide the ability to find annotations that are contained by another annotation.  The input is 2 Arrays of AQAnnotations.  We will call them A and B.  The purpose is to find those annotations in A that are contained in B.  What that means is the start/end offset for an annotation from A  must be contained by the start/end offset from an annotation in  B. We of course have to also match on the document id. We ultimately return an Array with 2 fields where the first field is an annotation from B and the second field is an array of entries from A
that are contained in the first entry.

**Before**  -  Provide the ability to find annotations that are before another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A that are before B. What that means is the end offset for an annotation from A must be before (or equal to) the start offset from an annotation in B. We ultimately return the A annotations that meet this criteria. A distance operator can also be optionally specified. This would require an A annotation (endOffset) to occur n characters (or less) before the B annotation (startOffset). There is also the option of negating the query (think Not Before) so that we return only A where it is not before B.

**After**  -  Provide the ability to find annotations that are after another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A that are after B. What that means is the start offset for an annotation from A must be after (or equal to) the end offset from an annotation in B. We ultimately return the A annotations that meet this criteria. A distance operator can also be optionally specified. This would require an A annotation (startOffset) to occur n characters (or less) after the B annotation (endOffset). There is also the option of negating the query (think Not After) so that we return only A where it is not after B.

**Between**  -  Provide the ability to find annotations that are before one annotation and after another. The input is 3 Arrays of AQAnnotations. We will call them A, B and C. The purpose is to find those annotations in A that are before B and after C. What that means is the end offset for an annotation from A must be before (or equal to) the start offset from an annotation in B and the start offset for A be after (or equal to) the end offset from C. We ultimately return the A annotations that meet this criteria. A distance operator can also be optionally specified. This would require an A annotation (endOffset) to occur n characters (or less) before the B annotation (startOffset) and would require the A annotation (startOffset) to occur n characters (or less) after the C annotation (endOffset) . There is also the option of negating the query (think Not Between) so that we return only A where it is not before B nor after C.

**Sequence**  -  Provide the ability to find annotations that are before another annotation. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A that are before B. What that means is the end offset for an annotation from A must be before (or equal to) the start offset from an annotation in B. We ultimately return the annotations that meet this criteria. Unlike the Before function, we adjust the returned annotation a bit. For example, we set the annotType to "seq" and we use the A startOffset and the B endOffset. A distance operator can also be optionally specified. This would require an A annotation (endOffset) to occur n characters (or less) before the B annotation (startOffset).

**Or**  -  Provide the ability to combine (union) Arrays of AQAnnotations. The input is 2 Arrays of AQAnnotations. The output is the union of these annotations.

**And**  -  Provide the ability to find annotations that are in the same document. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A and B that are in the same document.

**MatchProperty**  -  Provide the ability to find annotations (looking at their property) that are in the same document. The input is 2 Arrays of AQAnnotations. We will call them A and B. The purpose is to find those annotations in A that are in the same document as B and also match values on the specified property.

**Preceding**  -  Provide the ability to find the preceding sibling annotations for every annotation in the anchor Array[AQAnnotations]. The preceding sibling annotations can optionally be required to be contained in a container Array[AQAnnotations]. The return type of this function is different from other functions. Instead of returning a Array[AQAnnotation] this function returns a Array[(AQAnnotation,Array[AQAnnotation])].

**Following**  -  Provide the ability to find the following sibling annotations for every annotation in the anchor Array[AQAnnotations]. The following sibling annotations can optionally be required to be contained in a container Array[AQAnnotations]. The return type of this function is different from other functions. Instead of returning a Array[AQAnnotation] this function returns a Array[(AQAnnotation,Array[AQAnnotation])].

**TokensSpan**  -  Provides the ability to create a string from a list of tokens that are contained in a span. The specified tokenProperty is used to extract the values from the tokens when creating the string. For SCNLP, this tokenProperty could be values like 'orig', 'lemma', or 'pos'. The spans would typically be a SCNLP 'sentence' or could even be things like an OM 'ce:para'.  Returns a Array[AQAnnotation] spans with 3 new properties all prefixed with the specified tokenProperty value followed by (ToksStr, ToksSpos, ToksEpos) The ToksStr property will be the concatenated string of token property values contained in the span. The ToksSPos and ToksEpos are properties that will help us determine the start/end offset for each of the individual tokens in the ToksStr. These helper properties are needed for the function RegexTokensSpan so we can generate accurate accurate start/end offsets based on the str file.

**RegexTokensSpan**  -  Provides the ability to apply a regular expression to the concatenated string generated by TokensSpan. For the strings matching the regex, a Array[AQAnnotations] will be returned.  The AQAnnotation will correspond to the offsets within the concatenated string containing the match.

#### Concordancers

The following functions have proven useful when looking at AQAnnotations.  When displaying an annotation, the starting text for the annotation will begin with a green ">" and end with a green "<". If you use the XMLConcordancer that outputs the original XML (from the XML annotations), the XML tags will be in orange. The XML may not be well-formed). When generating annotations, you sometimes may want to exclude some text.  In AQAnnotations, this is done with the excludes property.  When an annotation is encountered that has an excludes property, the text excluded will be highlighted in red.  For more details on the implementation, view the corresponding class for each function in the com.elsevier.aql.concordancers package.  For usage examples, view the ConcordancerSuite class in the test package.

**Concordancer**  -  Output the string of text identified by the AQAnnotation and highlight in 'red' the text that was ignored (excluded).

**XMLConcordancer**  -  Output the string of text identified by the AQAnnotation and highlight in 'red' the text that was ignored (excluded). Also add the XML tags (in 'orange') that would have occurred in this string. Note, there are no guarantees that the XML will be well-formed.  Usage of the XMLConcordancer assumes you have processed XML and generated AQAnnotations for the XML tags.

**OrigPosLemConcordancer**  -  Output the string of text identified by the AQAnnotation (typically a sentence annotation). Below the sentence (in successive rows) output the original terms, parts of speech, and lemma terms for the text identified by the AQAnnotation. Usage of the OrigPosLemConcordancer assumes you have processed the content with an annotator (such as Stanford Core or Genia) and generated AQAnnotations for the POS tags.


#### Citing

If you need to cite AnnotationQueryLocal in your work, please use the following DOI:

[![DOI](https://zenodo.org/badge/194902260.svg)](https://zenodo.org/badge/latestdoi/194902260)

McBeath, Darin (2019). AnnotationQueryLocal [Computer Software];https://github.com/elsevierlabs-os/AnnotationQueryLocal
