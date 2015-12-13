# Textual Analysis of Reference Comments
Brian Gulbis  
December 11, 2015  


```r
library(pander)
library(stringr)
lor <- readRDS("lor.Rds")
```

## Improve


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}improv([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * Pharamcy department which helped to improve our services.  We certainly enjoyed
  * longer patient wait times and improve satisfaction. By the end of
  * always looking for how to improve herself. She always took feedback
  * able to utilize feedback to improve her patient presentations and performance
  * weaknesses that he wanted to improve upon during the course of
  * fantastic job with helping us improve the efficiency of the workflow
  * Keri continually strives to improve her abilities.   She accepts and
  * makes a genuine effort to improve her quality of work.
  * This program has resulted in improving the applicant pool for the
  * Stephanie showed improvement during her medicine rotation. This
  * feedback and seeks opportunities to improve performance. During her midpoint evaluation,
  * was able to observe considerable improvement in Stephanie's oral communication skills.
  * her peers and has shown improvement in initiating/problem solving clinical issues
  * any changes that could potentially improve her experience at FDA.
  * area Lauren will continue to improve in as she gains more
  * routinely.  She genuinely wants to improve and appreciates constructive feedback that
  * training and they continued to  improve throughout the duration of the
  * entitled "Evaluation and development of improved medication information handouts for tyrosine
  * accepts it in order to improve herself.
  * adequate and has room for improvement. Her organizational and time management
  * that is an opportunity for improvement for Jen.
  * above, she has room for improvement.
  * has made many strides in improving the timeliness of her communication.
  * feedback in an effort to improve her performance (as compared to
  * in seeking out ways to improve patient care without having to
  * constantly focusing on ways to improve her skills both indepentdently and
  * out to better herself and improve her skills as a pharmacist.
  * she can cite opportunity for improvement and validate her own self-evaluation.
  * However, she works diligently to improve her oral presentation skills. She
  * the feedback and work to improve her actions.  I never had
  * the suggestions which resulted in improved patient care.
  * always looking for ways to improve herself.
  * her peers and attempts to improve the overall success of those
  * an area that she can improve on with more experience and
  * made a concerted effort to improve on these things during the
  * She is always trying to improve herself and grow.
  * and enthusiastic in wanting to improve her knowledge and skills.
  * Crystal continued to improve her clinical problem solving skill
  * Crystal continued to improve her anticoagulation progress note documentation
  * has a passionate desire to improve clinically and personally, and he
  * he is always looking to improve in all aspects of his
  * evaluate them in order to improve for her next presentation.
  * there is still room for improvement.  During the last year of
  * interested in getting feedback to improve
  * Clinical skills improve quickly and steadily with time.
  * Needs improvement
  * constructive criticism and her performance improved throughout the rotation.
  * her and asked her to improve on these specific issues.  I
  * and demonstrated an eagerness to improve the quality of care for
  * allow her the opportunity to improve her skills as a practitioner.
  * but she has shown steady improvement throughout the year.
  * in return from them to improve the rotation for future students.
  * to solve drug-related problems to improve a patient's pharmaceutical care plan.
  * had regarding how he could improve even more were accepted very
  * always used the feedback to improve his performance.
  * time for patient work up improved greatly over the six week
  * improvement.  He takes constructive criticism seriously,
  * Always acted on suggestions for improvement.
  * Significantly improved during his tenure in my
  * ago, but he has significant improved in these areas.
  * language.  His writing skills have improved significantly since he came to
  * constantly sought out areas of improvement and feedback. Once given feedback,
  * so he is able to improve.
  * these characteristics to grow and improve throughout PGY1 residency.
  * He has led initiatives to improve processes for student organizations at
  * explanation as an area to improve upon.
  * poster preparation. His writing was improved significantly throughout the rotation and
  * what to work on to improve herself as a clinician and
  * acceptance and a determination to improve.
  * getting the results needed to improve our patients' outcomes without conflict
  * improvement
  * always looking for ways to improve and appreciates the feedback that
  * where she has room to improve and residency will really provide
  * build relationships in order to improve outcomes.
  * to choose one area of improvement for Janet, it would be
  * This is one area of improvement for Ms. Bliven.  She is
  * clarification on how she can improve things.  She was able to
  * she incorporated the areas of improvement that we identified.  She also
  * Addressed below in areas of improvement
  * modified things as need to improve.
  * Again, She will continue to improve here.
  * Actively sought out feedback to improve her performance
  * She will continue to improve here.
  * feedback to guide her in improving her performance, was open to
  * input on things she could improve upon.  Throughout her rotation we
  * in clinical decision making and improvement in clinical skills.
  * project) she could work to improve her eye contact. She completed
  * finding out what he could improve on.  During his time here
  * work so that she could improve her work down the road.
  * Ideally I would mark "needs improvement" for clinical problem solving skills
  * Sarah is clearly motivated to improve her skills and this is
  * She utilizes these feedback to improve personal and professional development.
  * patient-specific therapeutic plans. She consistently improves as the rotation progresses. She
  * is eagar to learn and improve.
  * Worked had to improve in this realm
  * in this area continue to improve.
  * share her suggestions for making improvements.  I do not see any
  * there was evident and significant improvement in the quality of his
  * witness his writing skills.  He improved throughout his rotation and became
  * on obtaining resources needed to improve patient outcomes.
  * new opportunities to learn and improve his knowledge base and clinical
  * made many important interventions and improvements to his patient's care during
  * and applies constructive feedback to improve herself and her work.  She
  * weekly manner on Fridays and improvements are reflected on the following
  * this area will be greatly improved.
  * always asking for ways to improve projects or how t o
  * Jennifer's problem solving skills consistently improved during the month and was
  * Her written communication skills consistently improved as the month progressed
  * always seek for areas of improvements and how she can further
  * the college.  She made significant improvements in the dinner and program.
  * always evaluating ways she can improve.  She is eager to learn
  * used it to work on improving.
  * to help her grow and improve.  She worked very independently, but
  * have watched her writing skills improve greatly over the past 3
  * and focus on areas of improvement throughout the rotation was commendable.
  * to incorporate the feedback to improve her performance.
  * and makes appropriate adjustments to improve her professional practice and learning
  * our pharmacy team helping to improve work flow and patient care
  * After our mid-point evaluation, Johnny improved markedly in the areas we
  * and advice on how to improve his quality of work.
  * would expect his initiative to improve in later rotations.
  * Johnny has some room for improvement.  During his rotation month with
  * one area where he could improve in.  He had never formally
  * that and was working to improve as the rotation progressed.
  * things that she needed to improve, always asked how she could
  * feedback that includes suggestion for improvement, she does not shy away,
  * could be an opportunity for improvement for Jennifer.
  * in learning of areas for improvement and she was always willing
  * seemed appropriate.  She made great improvement with seeing the whole clinical
  * Amber greatly improved in her ability to be
  * first clinical rotation.  She did improve throughout the rotation as her
  * Addressing her strengths certainly helps improve Shalin's confidence.  However, she is
  * of guidance initially, but did improve as the rotation progressed.
  * Shalin needs to remarkably improve in this area.  She did
  * notifying her preceptor.  She did improve after being reprimanded, but was
  * intervention. I feel this will improve once she becomes a licensed
  * she can cite opportunity for improvement and validate her own self-evaluation.
  * it that night. She greatly improved in her literature retrieval skills
  * we discussed some areas of improvements for her next rotation.   I
  * the rotation, I noted significant improvement in this skill.
  * patient care (example: how to improve the monitoring form, interpretation of
  * that she has rooms for improvement especially when she had lots
  * his patients, and he consistently improved throughout the rotation with being
  * good and will continue to improve with clinical rotations and residency
  * looking for ways she could improved during her research project.  She
  * Frequently seeks feedback to improve performance. Considers and incorporates feedback
  * Slocum is always striving to improve to be the best future
  * it so that she could improve in her work. When I
  * open to constructive feedback on improving her written and oral communication
  * excited by the opportunity to improve.
  * and was always willing to improve his approach.
  * criticism as he wants to improve his clinical skills. He handles
  * weaknesses and actively tries to improve upon them.
  * for feedback allowed him to improve his performance, but going forward
  * and case presentations were dramatically improved - no more distracting mannerisms
  * reason for the other students improvement in their performance.
  * allowed him to make significant improvements in this area. He should
  * perceived Lucas, but he did improve as the month went on.
  * These are common areas of improvement for residents even, and I
  * Colleen improved significantly throughout the rotation with
  * definitely has a drive to improve with everything she does.
  * constructive feedback in order to improve her performance and growth throughout
  * Her problem solving skills improved throughout the rotation.  She was
  * more organized flow. To help improve her interview style, she compiled
  * handouts.  I believe this will improve with practice and she did
  * approach, she was able to improve in this area halfway through
  * experience.  Her thought process did improve over the course of her
  * often asks for recommendations for improvement.
  * negative feedback and adapt to improve week after week.
  * and can identify opportunities for improving patient care.
  * We mainly discussed ways to improve his time management and prioritizing,
  * between his thoughts. This did improve with more revisions. By the
  * improvement was made evident on his
  * and actually seeks ways to improve and is constantly striving to
  * day two she had drastically improved.
  * in seeking out opportunities for improvement.
  * Once again Hilary could improve in this area but she
  * one area that Hilary could improve, which I discussed below.
  * and often seeks guidance to improve her writing skills.
  * learning experiences and strives to improve on a daily basis.
  * on things that she can improve on.  For example, Amelia has
  * described by the opportunities for improvement section below.
  * get the experience and expectedly, improved in assessing patient data working
  * her face and eager to improve patient care.  She is punctual,
  * all in a bid to improve the quality of service she
  * With a focus on process improvements, Ololade continuously seeks opportunities to
  * set.  This also led to improved interactions with patients and the
  * strong opinion that everyone can improve.  We had completed mock interviews
  * to work on them and improve. The best example I have
  * make a clinical assessment, Elizabeth improved in the effectiveness of her
  * rounds. Elizabeth's patient presentations also improved throughout her time with me.
  * for each patient also dramatically improved throughout the rotation. She would
  * to organize and prioritize tasks improved.
  * criticism and suggested means for improvement. If a deficiency was identified
  * Prisca is always looking to improve herself and asks for constructive
  * seeks advice and tips for improvement from me throug out the
  * her counseling skills continue to improve during the completion of her
  * These skills have continued to improve after the completion of my
  * is an area of continuous improvement. But I think with time,
  * them the best opportunity to improve and be successful.  She was
  * and other additional activities to improve her knowledge-base and a longitudinal
  * into his daily activities to improve.  He always has a good
  * seeks advice and feedback for improvement.  Many students don't do that
  * Mahmoud took advice for improvement well and showed he listened
  *     or how did you".

    Mahmoud demonstrated improved skill in raising questions or
  * some guidance. Has room for improvement of course, but is advanced
  * the rotation, his notes had improved greatly. He was beginning to
  * Mahmoud improved on his presentation style and
  * to NPs and MDs.  Some improvement throughout the rotation but still
  * than itemized problem list for improved efficiency, utilization of primary literature
  * she was able to significantly improve her communication skills from baseline.
  * She often seeks criticism to improve her outcomes over praise for
  * outside her comfort zone to improve her performance and increase her
  * amazing level of commitment to improving how she communicates. She has
  * was an alternative that would improve or increase a patient's quality
  * issues she believes she can improve.
  * actively makes the changes and improvements. I had the opportunity to
  * I was able to observe improvement in this area throughout the
  * Abeer wants to improve and be as good as
  * over the intimidation factor and improved her knowledge base to become
  * where she could continue to improve. We have had discussions about
  * Hoang's writing skills need improvement.  Due to her being from
  * is very focused on consistently improving his performance.  During my APPE
  * to feedback in order to improve his skills and outcomes. Brandon
  * and find a way to improve. I have not had issues
  * clinical case with underclassman to improve critical thinking skills.  This past
  * to manager her time has improved as she now manages to
  * was always seeking ways to improve

<!-- end of list -->

## Unfortunate


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}unfortun([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```

Quitting from lines 28-32 (text_analysis.Rmd) 
Error in x[[i]] : subscript out of bounds
Calls: <Anonymous> ... capture.output -> evalVis -> withVisible -> eval -> eval -> pander

## Struggle


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}struggl([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * I find is sometimes a struggler with PGY1 residents.  By the
  * to critically think. Most students struggled because of the Integrated nature
  * sessions for students who are struggling with calculations, workshops focused on
  * team-based learning environment.  While others struggled with the departure from the
  * training them in areas of struggle.
  * medical center but Tamara really struggled with this much independence. I
  * I struggle with my response to this
  * Helped a struggling student on rotation with her
  * a clinical standpoint, Joel initially struggled with presenting patients in a
  * devise an anticoagulation plan but struggled with providing a rationale as
  * P4 rotations. However, she has struggled with time management for some
  * Joe struggles with professionalism at times, particularly
  * daunting schedule, Joe at times struggled with time management. However, it
  * the ambulatory care rotation, Mahmoud struggled with writing complete and thorough
  * them the hard truth when struggling with weight loss, A1c, etc.
  * fearful of failure and avoid struggles. She continues to utilize the
  * skill than even now I struggle with - learning to say
  * his SCCP project that he struggled to meet some initial deadlines.

<!-- end of list -->

## Concern


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}concern([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * soft spoken, listens to patient concerns and answers appropriately.  Stephanie really
  * medication profiles, asks customers questions concerning allergies and other meds.  Will
  * facts/labs with patient and Intern/RPh concerns are passed on.  The provider
  * regard amongst the faculty for concern for her classmates and a
  * She listens to every patient's concerns and questions and takes her
  * for rounds and asked questions concerning patients prior to rounds.
  * and other health related patient concerns in a clear and concise
  * She demonstrates her sensitivity and concerns for others.
  * not observe any areas for concern with her ability to work
  * not observe any areas for concern with her level of professionalism.
  * advocate for her patients and concerns but will willingly accept criticism
  * Henry always brought his concerns to me about patients when
  * student that I was never concerned with allowing him to independently
  * are definitely not a major concern of mine, but this is
  * and shows empathy to patient concerns such as pain levels and
  * and appropriate. I had no concerns regarding her ability to write
  * Shows care and concern for others, operates on a
  * or opinions.  I have no concerns here, Mina has a great
  * health fairs. I have no concerns with Tiffany's ability to manage
  * to effectively communicate patient care concerns and potential interventions  during our
  * player and genuine compassion and concern for her patient's well being.
  * important patient and that their concerns are her concerns.
  * actively to their needs and concerns.
  * to override the opinion and concerns of others but did address
  * relaxed, and despite his initial concerns, I would rate his public
  * to intervene and voice her concerns in a timely manner.  She
  * My only concern is that she sometimes presents
  * concerns and often jumps in to
  * own ideas and stating his concerns which were often valid. When
  * listens to their questions and concerns and takes the time to
  * but I do not have concerns about her lack of ability
  * therapy problems. He showed great concern for patients as well. For
  * skill in raising questions or concerns about medication therapies. Encouraged him
  * responsibilities within the team.  No concerns about Annilee's ability to behave
  * and empathy, I have no concerns that she does well when
  * preceptor.  Daily, she presented her concerns and recommendations to me.  She
  * to be done to address concerns and to effectively initiate recommendations.
  * two years. There are no concerns whatsoever with regard to professionalism.

<!-- end of list -->

## Difficult


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}difficult([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * also had exposure to several difficult patients that she handled very
  * typical formats.   This made it difficult to conceptualize and it changed
  * maintaining an accurate view of difficult issues.  She possesses an easygoing
  * Can be given difficult tasks and will work diligently
  * ability and assist them with difficult course work.
  * see students succumb to these difficulties.  She was able to work
  * going through a lot of difficulty in her life, she still
  * Olivia had to face many difficult clinical situations. For example, she
  * She is able to handle difficult situations appropriately.
  * to understand, which is often difficult for some students.  He was
  * fellow pharmacists.   Some interns have difficulty removing themselves from the student/technician
  * information from even the most difficult patients, communicate findings to his
  * and sometimes this adjustment was difficult for previous interns, but not
  * handle this request and have difficulty putting such feedback into words.
  * bi-fold pamphlet.  This sometimes proves difficult for students to decide what
  * yet confident. This is a difficult trait to refine and William
  * times, even with the most difficult patients.
  * worked brilliantly together; collaborating on difficult clinical issues, splitting up the
  * difficult" attending physician or resident as
  * always picked up the most difficult cases that other students were
  * calm and professional even in difficult situations or in dealing with
  * all in stride and handled difficult tasks while still maintaining his
  * accent but she has no difficulty communicating orally.  I have no
  * seen her work well with difficult staff members.  Uses oral communication
  * of stress and in new, difficult situations. She conducted her research
  * eagerness to learn and grasps difficult clinical concept with ease. During
  * These are tasks that are difficult for many students to handle
  * Oncology is a difficult area for students because it
  * VTE.  This was a more difficult topic to cover and I
  * possible.  This may have been difficult for other students to do,
  * was overwhelmed.  She mentioned the difficulty of this rotation, doing her
  * depth because she would have difficulty with material she should already
  * poise and maturity, even in difficult or high-stressed situations.
  * general medicine rotation so its difficult to evaluate.
  * his month- that is a difficult challenge for residents (and practitioners
  * stable; she handles criticism and difficult situations with ease.
  * eagerly took on the most difficult patient cases to get the
  * Some of our most difficult patients warmed up to Nicole
  * of "I don't knows" and difficult for her to connect the
  * an accent, but is not difficult to understand.
  * common in clinical practice, but difficult to address in school.  Annilee
  * level of accountability. It is difficult to stand back and let
  * Pediatrics is known as a difficult rotation at our campus because
  * to address and appropriately handle difficult clinical situations with minimal interventions
  * caregivers.  She is patient in difficult situations and is never unprofessional
  * how challenging the situation or difficult the question. Even when dealing
  * as that information was previously difficult to find. The organization has
  * her presentation slides and handled difficult clinical questions after the presentation

<!-- end of list -->


```r
text <- grep("difficult[^y]", lor$comment, value = TRUE, ignore.case = TRUE)
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * Monica provided discharge counseling to every patient that she was following and left the unit during her APPE with me. With minimal modeling required, Monica was able to effectively communicate with patients and provide discharge counseling. She also had exposure to several difficult patients that she handled very well. She effectively documented her patient interactions in a timely manner.
  * Keri continually strives to improve her abilities.   She accepts and responds to feedback well.   The quality improvement project that she undertook was complex and did not adhere to typical formats.   This made it difficult to conceptualize and it changed much over time based upon feedback from many stakeholders.   Keri was flexible and able to adapt to this ambiguity and thrive.
  * Keri has the ability to communicate in a consistently upbeat manner while maintaining an accurate view of difficult issues.  She possesses an easygoing and efficient manner which facilitates the effectiveness of the entire team.
  * Can be given difficult tasks and will work diligently to solve them.
  * She served as a peer tutor in our supplemental instruction sessions.  I have personally observed her ability to work with students with varying degrees of ability and assist them with difficult course work.
  * Katie has face several significant challenges during her time in school.  In many cases I see students succumb to these difficulties.  She was able to work through them while moving forward with school and other responsibilities.  She was willing to seek help from school resources when necessary; however, it was in the desire to facilitate her learning and not seeking an easier route.  She is a very strong individual with a significant desire to learn and become a high level practitioner.
  * Within the ED, Olivia had to face many difficult clinical situations. For example, she was interviewing a patient who began to actively seize. She was always able to handle these emotionally taxing situations and focus on delivering optimal patient care during the event.
  * Lan always displayed the highest level of professionalism. She is able to handle difficult situations appropriately.
  * While on my rotation Emanuel did a wonderful job working with patients. He does well with patient counseling and patient education. This was noted during the several patient group discussions that he led on the inpatient psychiatric unit and the substance abuse rehabilitation program (SARP). He was also able to communicate important subject matter in a way in which the patients were able to understand, which is often difficult for some students.  He was very personable and the patients truly enjoyed his groups. Because of his outstanding performance, I let him run a majority of the groups independently.
  * Our Emergency Medicine rotation requires students to spend much of their day obtaining current medication regimens. Jason was very effective and professional in discussing medications/regimens with patients. He was able to obtain information from even the most difficult patients, communicate findings to his pharmacy preceptors and answer patient questions and/or provide counseling when necessary.
  * During Jason's first three years of employment, he was assigned to clinics and pharmacy operations, where he completed one tasks at a time and time management was not a priority. Over this last year, our interns are assigned to Medication reconciliation for admissions and discharges throughout the day. Time management is a high priority and sometimes this adjustment was difficult for previous interns, but not for Jason.  He enjoys making lists, managing his time, and prioritizing tasks.  Jason also keeps in contact with the assigned pharmacist to keep updated on changes and continually organize tasks based on workload.
  *     In an era of electronic communications good written communication skills have diminished; however, this is not the case with Mark. He possesses good interpersonal writing skills which have enabled him to compose quality correspondence. His written interpersonal communications, primarily in the form of emails, are appropriately professional in both content and style.

    As for his clinical writing, I have primarily observed Mark's abilities within the context of a drug information rotation.  While most papers during the drug information rotation are team-written, Mark had the opportunity to independently author, meaning he was the only student involved but with faculty editorial oversight, two formal writings.  One was an educational brochure (Nursing Know & Go) on tedizolid (Sivextro) on behalf of the P&T committee at a 200+ bed regional medical center.  This task required Mark to identify and distill key pieces of nursing-related drug information about the drug along with a 5-question post-test and summarize it in a one-page bi-fold pamphlet.  This sometimes proves difficult for students to decide what to include and how to effectively convey the information in a brief communication, but Mark did an excellent job that required only limited editing and the P&T committee chairman was pleased with his work.  The other was a public health oriented article entitled "Antibiotics: To Need or Not to Need" that Mark authored for Searcy Living which is a regional community magazine.  The article was aimed at helping patients better understand concepts related to proper antibiotic usage.

    Mark also sought out the opportunity to develop more formal, technical writing skills by participating in the preparation of a manuscript in which he summarized the results of a methadone-related medication errors research project.  For this manuscript Mark was not charged with any primary responsibilities but his efforts demonstrate he recognizes the value of developing professional writing skills across a variety of mediums.  In all instances, Mark's writing was accurate, well-organized, reasonably complete, and appropriately insightful.

    Finally, Mark has received exposure to critiquing the writing of others through his participation in the editorial review of poster abstracts presented at the ASCP annual meeting (2014), as well as the editorial review of a book chapter entitled "Biotechnology and Personalized Medicine" in a book on pharmaceutical public policy (Jones & Bartlett Learning, 2014).
  * William has great oral communication skills. He has a very laid back and down-to-earth personality which makes him very easy to talk to and get to know. When making recommendations to physicians he was always very respectful yet confident. This is a difficult trait to refine and William is ahead of his peers in his ability to communicate effectively with multiple healthcare disciplines.
  * William always comes to work dressed to impress and he keeps a professional attitude at all times, even with the most difficult patients.
  * During her rotation, I also precepted a pharmacy resident.  Bethany embraced having another peer/mentor in her midst and they worked brilliantly together; collaborating on difficult clinical issues, splitting up the task of rounding with multiple ICU teams, and sharing the overall daily workload.
  * Rita never asked me to help her communicate a recommendation to certain "difficult" attending physician or resident as many students do when they are intimidated by those physicians.
  * Rita leads by example. Her rotation mates always looked up to her. When she had the turn at distributing the patients to her rotation group, she made sure she took care to make sure it was fair and she always picked up the most difficult cases that other students were reluctant to take. I frequently noticed that she was assisting her rotation mates who needed help yet always completing her work on time and never in a hurry to leave at the end of the day even though she was always to the first to arrive in the morning.
  *     Very mature and professional approach to all of her interactions.
    She was calm and professional even in difficult situations or in dealing with difficult or demanding patients. Always professional attire and always a professional approach to all of her interactions and responsibilities.
  * Li has shown great emotional stability and maturity throughout my time with him.  His third year was based in San Antonio while I am in Austin.  He was able to tackle a new student initiative, Know Your Medicine, and enhance the research associated with it while working with me long distance.  Events and deadlines were accomplished successfully and on time.  I know there were times that were stressful for him, but he took it all in stride and handled difficult tasks while still maintaining his grades and coursework.
  * Jenny gets along with all of the staff, very active at the school.  Everyone is glad to see that Jenny is the pharmacy student coming into work.  She brings issues immediately to person in charge and often with ideas for dealing with the issues.  I have seen her work well with difficult staff members.  Uses oral communication as well as written communications effectively.
  * Mina's level of professionalism is exceptional. Of particular note is her ability to maintain this level of professionalism even during times of stress and in new, difficult situations. She conducted her research at my practice site, the Joslin Diabetes Center. Joslin is a Harvard-affiliated institution, which means the level of professionalism, knowledge and expertise expected can be significantly difficult for most students to achieve. Mina was not only able to achieve these expectations, but impressed both me and the Harvard Medical School faculty we worked with on her project.
  * Tiffany is a very bright individual who has consistently done extremely well in the classroom setting. She expresses interest in a wide-variety of topics and seems to have an aptitude for everything that comes her way. She is one of those students who often asks additional questions for clarification and in doing so expands the conversation so it benefits the whole class. She has a genuine eagerness to learn and grasps difficult clinical concept with ease. During facilitated discussions in our Comprehensive Patient Care course, Tiffany often demonstrated an advanced clinical understanding beyond her classmates that I often don't see in students until they are well into their PGY1 residency.
  * Tiffany is incredibly organized and is excellent at multi-tasking and time management. She is currently working as an intern at a community pharmacy, and holds membership in many organizations including, APhA-ASP, ACCP, CSSHP and the Academy of Managed Care Pharmacy (AMCP), as well as our Phi Delta Chi Chapter. These are tasks that are difficult for many students to handle in addition to our strenuous curriculum; however Tiffany has maintained a strong GPA. She is also very involved in the overall promotion of health and pharmacy through her participation in numerous community health fairs. I have no concerns with Tiffany's ability to manage many tasks required of a PGY1 residency with ease.
  * Oncology is a difficult area for students because it is a very complex field.  His answers were not always right, but he was always on the right path.  By the next day, he consistently had detailed follow-ups which corrected any incorrect responses or answers that he did not know at the time.  He was genuinely interested in learning and understanding whatever was presented to him.
  * At the start of the rotation, Jeena mentioned that she had an interest in further developing her clinical knowledge related to infectious disease and venous thromboembolism.  Instead of avoiding topics that she considered herself to be weak in, Jeena challenged herself by making sure to focus on these areas whenever possible.  She modeled her one-hour CE presentation for the nursing staff to cover VTE.  This was a more difficult topic to cover and I was extremely impressed with the end result.
  * Jeena was on rotation with another student who was not as strong as her.  I observed her mentoring the student to ensure that they were both successful.  I think she did a great job of allowing the other student to learn and grow while at the same time ensuring that she too got as much out of the rotation as possible.  This may have been difficult for other students to do, but I was very impressed with how tactful she was.
  * Minoosh shows tremendous poise and maturity, even in difficult or high-stressed situations.
  * Likhitha did well on the in and out of class patient case assessments, but I have not had her on my general medicine rotation so its difficult to evaluate.
  * This is easily demonstrated by all of Lucas's activities: a rigorous 4th year rotation schedule, active research, leadership positions, as well as working in the UK intern program all throughout. His ability to organize time on rotation was equally impressive. He wasn't quite able to follow all patients in the MICU, although the census had started to balloon upward of 20 patients or more by the end of his month- that is a difficult challenge for residents (and practitioners at times).
  * Hilary is one of the most mature students I have precepted.  She is emotionally stable; she handles criticism and difficult situations with ease.
  * Difficult to judge during a 6 week rotation, but no negative observations.
  * As this was her very first rotation, Amelia asked insightful questions from the very beginning, exemplifying her solid knowledge base and highlighting her interest in learning.  Throughout the rotation, I observed Amelia while rounding with the multidisciplinary cardiology team, comprised of medical residents, a cardiology fellow, nurse practitioner and attending.  The team would often look to her for assistance with pharmacotherapy during rounds.  Amelia has a great understanding of pathophysiology and baseline knowledge of pharmacotherapy. She actively participated in rounds, often offering rational, evidence-based interventions on her patient's medication therapy plans.  Considering this was her very first APPE rotation, Amelia eagerly took on the most difficult patient cases to get the experience and expectedly, improved in assessing patient data working to identify medication therapy problems, including assessing disease state management, adherence and evidence-based guidelines, throughout the 6 weeks.  Additionally, I saw a huge improvement in her independence and taking appropriate self-initiative to assess the pharmacotherapy plan for appropriateness.  Her monitoring and follow up of therapeutic recommendations were consistently reliable and complete.
  * Some of our most difficult patients warmed up to Nicole after only a few interactions. They believed in her abilities and she proved them right.
  * Elizabeth's ability to think critically stands out when I think of all the students I have had the pleasure to precept. My MICU rotation was Elizabeth's first patient care rotation. I expected for her to be full of "I don't knows" and difficult for her to connect the dots and see the big picture, but she proved me wrong! Elizabeth has an impressive drive for learning. She really enjoys obtaining new clinical knowledge and being challenged with patients. I pushed her every day and provided ample opportunities for repetition so that she would retain information. I challenged her to look for the "why" in every scenario and this allowed her to flourish in her knowledge. Her ability to present patients and make an assessment and plan for each patient also dramatically improved throughout the rotation. She would not only present the problem but the supporting objective evidence and a thought out plan. She is not afraid to ask questions but makes every effort to find the answer first. In addition, she began to dive into primary literature and she was able to develop a deep understanding of multiple ICU topics. She vastly improved in her clinical knowledge and critical thinking skills over her 6 weeks on my rotation.
  * Prisca has an accent, but is not difficult to understand.
  * We had a challenging rotation in this regard, and Annilee was able to benefit from experiences that I don't believe common for most pharmacy students.  Annilee was involved in the management of quite a few cardiac arrests, as well as management of many patients being evaluated for transplant/cardiac support device vs. palliative/comfort care.  Annilee is much more emotionally prepared to deal with these sorts of end-of-life and triage decisions that are common in clinical practice, but difficult to address in school.  Annilee handled these situations admirably, respectfully, and was able to use each of these cases as a learning opportunity to recognize practical alternatives and risks of therapy in critically ill patients.  Annilee being able to function in this sort of environment bodes well for her ability to handle stressful situations and rotations that she will encounter as a resident.
  * Elizabeth has a great personality and extends a great deal of trust to others to work to the level that she does. Although this has created frustrations for her depending on the rotation, peer or project she was involved in, she continues to extend to others what she wants for herself and holds everyone to the same level of accountability. It is difficult to stand back and let her experience this frustration, but it is because of her incredible willingness to work through any situation, that she will find something positive to focus on and make the best of the resources and people she has. She is very organized and has the ability to articulate what she needs and what her expectations are for her team or group, and allows the process of delegating to occur, which is a challenge for most students. She genuinely believes that by setting an example to follow, that others will work to her same level of commitment and will remain as committed to the goals and objectives that she is. She is culturally competent, has an appreciation for diversity, and this is apparent by the activities she has led, the students she has recruited, how she has communicated and what she has achieved. I believe that when matched up with colleagues with the same level of integrity and work ethic that she will continue to thrive in the clinical environment created by a balanced and sound residency team. She is so excited to work with other new graduates and pharmacists with expertise so that she can continue to finesse her approach to clinical problem solving and continue to foster her own unique style and approach to patient care.
  * Hoang has really excelled in her clinical problem skills.  I have seen tremendous growth in her since she was  a third year.  The majority of my interaction has been on the pediatric rotation, but I really pushed her in that rotation and she did extremely well.  Pediatrics is known as a difficult rotation at our campus because of the high expectations I have of our students.  She did extremely well.  She also did well in the physician rotation that I coordinated for her.  I felt comfortable that after her time with me she would be very effective when she was on the physician only rotation. I am extremely proud of the progress she has made. She really developed the strong critical thinking ability.
  * Hoang is able to address and appropriately handle difficult clinical situations with minimal interventions by her preceptors.  I have observed Hoang in many different patient consultations.  The general pattern of these includes her thorough assessment of the situation, her ability to clearly define the medication-related problem, her resourcefulness in determining an appropriate recommendation, her clarity in communicating this message to the patient and her compassion for the patient throughout their interaction.  Hoang makes it a point not to guess what the appropriate clinical recommendation would be.  She relies heavily on established clinical recommendations for dosing, evaluation of adverse effects and expected therapeutic outcomes.  Hoang generally seeks consultations with her preceptors for assistance in making recommendations in only the more difficult or complex patient problems.  Even in these situations, she actively engaged in the discussion with the preceptor, and does not simply ask for the preceptor to solve the problem.  She respectfully shares her appraisal of the situation and the direction she is leaning in her analysis of the situation.  When the recommendation of her preceptor does not match her own ideas, she respectfully seeks to understand why the preceptor made that recommendation.  In this way, Hoang continually develops her own clinical problem solving skills.
  * Hoang is consistently professional both in her attire and her demeanor.  She is respectful towards her preceptors and other staff members.  She is especially professional in her interactions with patients and caregivers.  She is patient in difficult situations and is never unprofessional in her interactions with patients --even when the patient's demeanor is angry or aggressive.  She is a model of assertive problem solving in these situations.  I am impressed at her ability to be exceptionally professional in both low and high stress situations.  Professionalism is one of her strengths.
  * Brandon is very mature.  He never displaying any instances of emotional instability despite the stressful environment of the ICU.  On the contrary, he was always calm, and maintained a collected demeanor no matter how challenging the situation or difficult the question. Even when dealing with difficult surgeons, Brandon was always poised, and he represented pharmacy well.
  *     I had the opportunity to work with Brandon for 2 years as he served as president of the Student College of Clinical Pharmacy (a student chapter of the American College of Clinical Pharmacy). Our informal student chapter was established during the 2011-2012 academic year.  However, due to various challenges (including 17 other student organizations at our College), we had a slow start to building up the membership.  As a matter of fact, we began the 2012-2013 year with no officers.  Brandon quickly approached me when we announced elections to run for President.  He had researched ACCP and had a strong understanding of what made the organization unique.  Quickly after his election, our membership began to grow. Brandon has led SCCP-NSU to become the `go-to' organization for professional growth and development and unique opportunities related to clinical pharmacy on our campus. After his first year as president, it was decided to re-organize the leadership structure to include a president-elect for continuity and the organization was fortunate to have Brandon stay on as president for a second term.

    I can't share all of the successes we have had in those two years, but I will highlight some of the organizations successes under his leadership. The first was the restructuring of the leadership team. In two years, we now have over 15 leadership positions with multiple candidates running for each position. Despite having so many students involved in various functions, Brandon managed to engage each student (even at different campuses) and maintain communication regarding our numerous events and functions--which at times overwhelmed the advisors!  The development of the fundraising committee has provided the organization financial stability and has allowed us to offer lunch at most of our events, which is unheard of for such a new organization. Of the 18 organizations, SCCP-NSU consistently ranked in the top 5 most active in terms of community outreach, guest speakers, and professional development. The organization implemented an on-line student-managed website for introductory and advanced practice experiences for underclassman to use in deciding which rotations best suited future career goals, as that information was previously difficult to find. The organization has strengthened the Research Mentoring Program which pairs up students with faculty mentors to complete a publishable research project in 1-2 years.  We have a regularly occurring Case Lunch Series in which upperclassman are provided opportunities to work through a clinical case with underclassman to improve critical thinking skills.  This past spring, the organization sponsored a Clinical Pharmacist Roundtable (similar to what was done at the annual meeting in Albuquerque) with local ACCP members. The event was well attended by both Clinical Pharmacists and students.  It was described by some as "the best event in his 2 years of pharmacy school." This is a sampling of what SCCP-NSU has accomplished under his leadership. He served with humbleness, engagement of all officers and members, and enthusiasm.
  * Stephanie presented a complicated Diabetic Ketoacidosis patient from the Emergency Department to a panel of pharmacy clinical specialists.  She required very little coaching while developing her presentation slides and handled difficult clinical questions after the presentation from different clinical pharmacy specialists.  Stephanie was able to utilize the appropriate resources and literature to present and field questions over a topic she had not had much experience with.

<!-- end of list -->



## Not


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}(did|could|would|does|was|has|had|will|should|can) not( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * Kristi did not shy away from our retail
  * she undertook was complex and did not adhere to typical formats.   This
  * example, one of her patients did not have a measured drug level
  * she may be soft-spoken, this does not hinder her ability to communicate
  * consult the provider is something does not seem right.
  * for students to procrastinate. Stephanie did not fall victim to this and
  * medical team was grateful. She did not back down and was not
  * Krishna was very professional and did not exhibit any signs of immaturity
  * Lauren is appropriately assertive.  I would not consider her overly assertive, but
  * the room and that she was not only well liked but well
  * would bring any questions she could not comfortably answer to me to
  * area in which Lan Bui has not exceeded the skills of all
  * communications and interactions that I would not feel comfortable trusting other students
  * she was unsure about. She was not afraid to speak up in
  * No misspellings or incorrect grammar was not
  * through numerous leadership roles. I did not personally have the chance to
  * very mature and stable. She does not let things get to her
  * answer the question.  If information was not documented or what was documented
  * I did not serve as preceptor for any
  * I did not serve as preceptor for any
  * Veronica has not only demonstrated excellence in her
  * across a procedure/condition that she did not understand she would look it
  * not take criticism personally and does not get overly discouraged.
  * poster presentation was well-reviewed. I will not have him on rotation until
  * I will not have him on rotation until
  * him.  From his CV you will not
  * counseling and patient education. This was not
  * above patients with respect. He did not hesitate to participate in the
  * a time and time management was not a priority. Over this last
  * the emergency department that he was not able to constructively fill, even
  * recommendations to the team she did not hesitate and spoke with confidence.
  * she knew the answer but was not totally confident that the answer
  * I was not able to observe this characteristic
  * Kristin did not have much opportunity to interact
  * I did not observe any areas for concern
  * I did not observe any areas for concern
  * loyal to a fault and does not like to disappoint or be
  *     able to distinguish information that was not pertinent to a decision or
    solution.
  * project.  For this manuscript Mark was not charged with any primary responsibilities
  * Overall, this did not appear to be an issue
  * As an interntional scholar he has not had the same opportunities in
  * the questions and when they could not answer them, humbly volunteered all
  * Medicine students on service.  She was not intimidated by the medical students
  * and professionally assertive personality that does not seek the spotlight or all
  * as a volunteering mentor (it was not required, but he volunteered to
  * a quiet, competent leader.  He does not see accolades and is quick
  * at which I work.  She did not realize that I worked there
  * came through my store, she did not need to be spoon-fed information
  * He is a multitasker and does not get distracted easily.
  * into the medicine team--although they had not worked with PharmD interns before,
  * Her rotation was not very heavy in patient counseling
  * any healthcare team.  Her nervousness does not detract from her confidence, knowledge
  * or information that Walgreens simply does not allow us to provide.  It
  * I did not have interaction with Cody during
  * I did not have interaction with Cody during
  * While I did not serve as Cody's preceptor on
  * she left my rotation she was not exactly "acceptable" for this objective
  * When a question arose, Jozy would not hesitate to seek out the
  * Strong characteristic- she does not hesitate to speak up and
  * who commuted to school, there was not time for her to develop
  * life.  She meets deadlines and will not
  * STICU rounds. During rounds, he did not have any problem approaching the
  * patient. As mention before, this was not
  * up on the details and does not see the overall picture, which
  * her comfort zone.  She really did not require any handholding.  I witnessed
  * LaTasha was not afraid to ask for what
  * had her academic priorities and was not afraid to tell me she
  * Michael was assertive but did not present over confident to any
  * with the medical team and did not hesitate to approach nurses with
  * she evaluates how/why her best was not accomplished.  She wants to continually
  * I did not have the opportunity to observe
  * was a research rotation, I did not assess her patient interactions.
  * Mina's research project did not require interactions with patients.
  * more indirect assessment because she was not on a clinical rotation with
  * most students to achieve. Mina was not only able to achieve these
  * I did not observe any of Johnny's patient
  * While we did not have many opportunities for patient
  * responses or answers that he did not know at the time.  He
  * completed everything. Without him i would not have been able to complete
  * includes suggestion for improvement, she does not shy away, but learns from
  * rotation with another student who was not as strong as her.  I
  * I was not provided with the opportunity to
  * challenges as this year's group was not as excited about the extra
  * I did not have the opportunity to work
  * this Critical Care setting, it did not afford enough opportunities to dialogue
  * I did not have the opportunity to work
  * improve in this area.  She did not demonstrate mastery of basic anatomy
  * is still a student she was not as assertive as I would
  * I did not assess Minoosh on any writing
  * references (i.e. Lexicomp), but, initially, did not routinely go take the next
  * me.  I told Sarah I was not familiar with any evidence of
  * I did not evaluate any formal writing assignments
  * to note that her efficiency does not adversely affect the quality of
  * Christine was not only willing to accept constructive
  * Based on our conversations,she does not strike me as the most
  * coordinator for APPE rotations, I was not Likhitha's direct preceptor and can
  * a critical care rotation, I was not able to observe Lucas with
  * with me this year. This does not mean that he sacrificed the
  * the answers to information she did not know.
  * to look up information she did not know and I believe she'll
  * even on days where things did not go as planned and we
  * drug information paper as I was not the primary preceptor for that
  * to meet the requirements. She does not hesitate to spend more time
  * APPE rotations even though this was not required of him. He did
  * professional email, but personally it was not always up to my standard.
  * humble in admitting when he does not know something and is diligent
  * Medicare patients was superb. I did not analyze her clinical cases. That
  * and attentive to details.  She was not afraid to ask questions in
  * involved another student. This student was not as strong as Elizabeth in
  * improved throughout the rotation. She would not only present the problem but
  * research project with me. I did not have any issues receiving drafts
  * accurate at all times.  I did not feel the need to review
  * her preceptor. Most questions asked did not have a right or wrong
  * well researched, and professional.  We did not really have other opportunities to
  * This was not directly assessed on rotation, as
  * critical care environment that she had not been previously exposed to.  However
  * to during her training.  Annilee did not succumb to "information overload" and
  * that criticisms will come but will not cause her to falter or
  * Michelle was always professional. I can not think of a day I
  * she gets to work, she does not passively wait to be told
  * and makes requested changes.  She does not get defensive when I have
  * discussion with the preceptor, and does not simply ask for the preceptor
  * I will not have Brandon on a clinical
  * I will not have Brandon on a clinical
  * feedback on this issue. She had not realized that she was doing
  * questions over a topic she had not had much experience with.
  * from many residents. Additionally, she did not need a lot of direction
  * he arrived for rotation. He would not wait for me to tell

<!-- end of list -->


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}(didn\'t|couldn\'t|wouldn\'t|doesn\'t|wasn\'t|hadn\'t|hasn\'t|won\'t|shouldn\'t|can\'t|cannot)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * necessary.  This is something that cannot be said about all of
  * are saying and if she cannot answer something she will utilize
  * a miscommunication with Danna, she didn't let it go and she
  * (a newer faculty member she hadn't met) after a presentation I
  * I cannot comment on this field because
  * on rotation as well.  I cannot recall a time of her
  * as passive to someone who doesn't know her. Dana is actually
  * not a clinical preceptor, I cannot evaluate this skill.
  * game on TV, so I cannot comment in-depth on his emotional
  * to find information if he doesn't know something.  He is very
  * Although I wasn't able to observe William directly
  * solving skills whenever his experiments didn't go as planned. He was
  * of the medical team and didn't ever appear to be too
  * complete them all. If we didn't get to all of the
  * best possible pharmaceutical care.  I cannot recall a time ever when
  * of her own if I didn't present her with one. Compared
  * He looks up what he doesn't know, asks appropriate and thoughtful
  * and always ask for. I cannot elaborated or contribute on his
  * although she is quiet, she doesn't get run over--she knows how
  * a strength of Shiyi's.  She doesn't just read and regurgitate review
  * respond by informing me they cannot address this problem until they
  * afraid to tell me she couldn't work because she had an
  * constructive criticism was something she didn't generally welcome.  Now, she uses
  * her journal club presentation.  She didn't seem nervous during the presentation
  * have done such, if Amber hadn't risen the bar so high.
  * to appropriately problem-solve when things didn't go according to plan.
  * I really can't assess.
  * high level of confidence. I couldn't believe the transition. He said
  * rotation was equally impressive. He wasn't quite able to follow all
  * Joel in clinic, thus I cannot provide any input on the
  * versus intentionally non-adherent. While I cannot speak on his clinical problem
  * to answer (aka information she didn't know) in order to come
  * to look up information she didn't know about diseases or drugs
  * as a reference/resource when I wasn't immediately available to field a
  * Mary doesn't need someone looking over her
  * intervention.  In situations where he wasn't firm on his position was
  * instead of merely stating she didn't know what to do in
  * to a medical resident that won't embarrass them in front of
  * learning opportunities on patients she wasn't following.  This allowed Annilee to
  * a feeling person - she doesn't want anyone to be upset
  * a patient may experience. You cannot ask for anything more than
  * isn't distracted by attention and doesn't work for recognition. She is
  * correspondence with others when she doesn't need anything from them. She
  * know and accepts what she doesn't with a willingness to find
  * their behalf, even if they cannot speak. She takes into consideration
  * and beneficial, even if it wasn't the preceptors same intervention exactly.
  * actions. She understands professional behavior doesn't end when the shift ends,
  * to managing her time. She doesn't take on additional tasks unless
  * drugs, or any therapy she didn't know and brought the information
  * does show frustration when he doesn't meet his goal, he is
  *     for a second term.

    I can't share all of the successes
  * can honestly say that I didn't have one single issue like
  * I was impressed given he hadn't completed the entire pharmacotherapy course

<!-- end of list -->

## Never


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}never([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * well as dependability. I have never had a problem with Russell
  * and manage time. I have never had a problem with Russell
  * her time. An extension was never required for any of her
  * Highly dependable, extremely resourceful, never cut corner.
  * never necessary.  This is something that
  * Sometimes quiet and never causes workplace problems.  When a
  * care activities were completed. She never once complained about work hours
  * to facilitate her discussion while never reading directly off the slides.
  * an assertive person. She has never been afraid to speak up
  * very vocal and is usually never afraid to speak up during
  * this personally because I have never had the opportunity to observer
  * writing skills and I have never questioned her ability to write
  * on time or early, and never appeared
  * to improve her actions.  I never had to give the same
  * her from the providers. I never had to prompt Kirby to
  * some people may, and she never lets a bad encounter or
  * I know that our technicians never hesitate to ask for her
  * was the only student to never take notes during class, rather
  * never on duty.
  * was good or bad. She never backed down from a challenge.
  * reconciliation for inpatient admissions.  Jason never takes his independence for granted,
  * in his demeanor. I have never observed or heard of any
  * assertive at appropriate times, and never in an overbearing manner.  Please
  * a mature individual. He has never shown signs of instability and
  * from rationality and I have never observed any errant behavior, uncontrolled
  * student/intern I've ever had.  I've never had an issue with him
  * strict confidentiality.  Summarily, I have never observed him to behave in
  * William was never inappropriate with others.  He had
  * he needed to research.  I never had to worry about him
  * never his experiments didn't go as
  * a student that I was never concerned with allowing him to
  * to work with.  I have never witnessed her complain about work
  * Rita never asked me to help her
  * her work on time and never in a hurry to leave
  * in working up her patients never seemed rushed or behind. Never
  * uncertain about anything.  She also never asked a question without already
  * never there is a drug related
  * in stride and I have never felt like I offended her
  * them argue and bicker.  I've never experienced any of this with
  * Sarah never complained about her workload and
  * weeks), she worked independently but never assumed anything. When a question
  * There was never a single incident that required
  * her greatest strengths.  I have never seen her anxious or flustered
  * never I had a suggestion or
  * deadlines for smaller projects.  Assignments never fell through the cracks but
  * liked and well-spoken and has never had any professionalism issues. He
  * could improve in.  He had never formally answered a DI question
  * development of new employees.  He never hesitated to share any insights
  * other student whom he had never met before.  By the end
  * Jennifer never seemed to get overly stressed
  * never possible.  She modeled her one-hour
  * with  her because it is never always about her team.
  * in my elective class.  She never seems to be at a
  * detailed and thorough that I never had to ask her to
  * after being reprimanded, but was never consistent.
  * allowed her to flourish.  She never tried to override the opinion
  * patients, something that I have never allowed a student to do
  * mature and friendly young woman never appears to be stressed. who
  * an extremely hard worker and never wasted a minute during the
  * correct patient.  His work is never left behind for the next
  * effectively.  Despite the workload, she never seemed to be stressed or
  * I have never had any interpersonnel issues with
  * to her scholastic duties. She never missed a deadline with me.
  * stable and mature. I have never seen stress get the better
  * my standard. Importantly, Joe was never offensive or rude, and in
  * truly say that I have never seen a student take as
  * never possible.  She recognizes that patient
  * the SICU team.  However, she never left  with a question in
  * also volunteers to help and never complains.
  * never the students made mistakes, she
  * and report back. She has never let her reservations to speak
  * memorize. She has shared this never worked for her and although
  * and her academic work has never been compromised. She is quite
  * but is always professional and never seems overwhelmed despite her many
  * in difficult situations and is never unprofessional in her interactions with
  * Brandon is very mature.  He never displaying any instances of emotional
  * I have never had or heard of a

<!-- end of list -->

## Needed


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}need([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * customers/patients, especially if the customer needed extra assistance. She was well
  * utilize appropriate language and jargon needed for the assignment.  Oftentimes, students
  * instruction for projects. All I needed to provide was the idea
  * previous rotations or researched as needed. She used appropriate sources for
  * limited direction from me and needed little revision after her completion.
  * needs to be more outspoken
  * needs to be a bit more
  * team. However, I felt she needed to speak up more during
  * to be told what she needs to do. Neither does she
  * The provider gets what they need to assist them in making
  * and appropriate to meet the needs of the reader. For example,
  * ask preceptors for help when needed. She would always have some
  * prioritize her most urgent patient needs first and completed her work
  * pharmacist.  She realizes that she needs to work on being more
  * ask for anything that she needs.
  * or seek other tasks that needed to be completed.
  * includes pertinent information concisely and needs little guidance with starting her
  * much initiative.  She understands the need for continued learning, for experiences
  * Speaks up when needed
  * make the appropriate changes when needed .
  * on her own and when needs assistance, readily asks for it.
  * seek out feedback from a need for reassurance).
  * review and revise them as needed before they were to be
  * the day or overnight that needed to be addressed immediately.  Could
  * to precisely communicate what she needs to any level of patient
  * speaking up when the team needed particular information (i.e. culture results)
  * thru with anything they may need. She goes above and beyone
  * the attributes a resident candidate needs to have to be successful.
  * specifically for Irina if they need any advice or a simple
  * not assertive.  We discussed the need to be able to step
  * excellent job independently researching information needed and turning in materials early
  * to find the information she needed to complete her assignments and
  * or offer additional explanations when needed depending on her audience (both
  * her knowledge with those who need it, and through her positive
  * is able to communicate her needs to her supervisors. I think
  * in different ways if she needs to.
  * its complications. I emphasized the need to recall previous course lectures
  * helpful to family member both needing directions and those on the
  * Ms. Kamakuru will need assistance in reviewing writing assignments.
  * search out what information she needed.
  * on thier own but when need be stand up and take
  * to get the information she needed in order to make her
  * very professional manner prioritizing which needed to be done first. She
  * had very professional writing and needed little editing for her written
  * on the project fully and needs very little guidance.
  * When working to meet the needs of her patients for discharge
  * questions to respond to additional needs identified during an initial interaction.
  * more efficient in finding data needed in problem solving.
  * to address the drug information needs of the caregiver of an
  * both master the information he needed to learn while helping others.
  * appeared wrong or incomplete, or needed verification. Mark was also able
  * on or something that he needed to research.  I never had
  * information, but really anticipates the needs of each patient and customizes
  * At times needs minor direction.
  * in order to get the needed information to make an appropriate
  * respectful and thoughtful of patients needs and is comfortable working with
  * community outreach event serving our needy populations.  Austin reached out to
  * easy to understand manner but needed to be fine-tuned slightly to
  * how to state what she needs in a way that is
  * During this process, she has needed very little prodding to keep
  * her writing skills were impressive, needing very little editing or correction.
  * and to research options.  Sometimes needs additioanl guidance to understand implications
  * of the feelings and the needs of those around her. Rita
  * While I don't recall needing to give Rita constructive criticism,
  * staff when additional information was needed.
  * assisting her rotation mates who needed help yet always completing her
  * as a second language; Will need polishing but not a limitation
  * assignment and clarifying questions as needed; Consistently completed projects ahead of
  * residency and that she will need to find ways to meet
  * as a second language;  Will need polishing but not a limitation
  * she will do whatever she needs to in order to reach
  * Very rarely needs feedback with her exceptional work.
  * intuitively perceive when a patient needs additional help understanding, and her
  * my store, she did not need to be spoon-fed information or
  * the writing alone.  Her work needed little to minor revisions throughout
  * carefully and modified things as need to improve.
  * y indication but do they need to be???). he received this
  * to ask for help when needed.
  * but essential things that are needed to run a pharmacy.
  * Does not need her hand held--knows how to
  * expectations for a student.  She needed almost no prompting from my
  * but remain available if they need me.  In the last 5
  * when drug information resources were needed, she excelled in retrieving/interpreting that
  * coursework and sought help when needed. She asked appropriate questions which
  * complete a medication history she needed very detailed step-by-step instructions on
  * needs improvement" for clinical problem solving
  * beginning of the rotation, Sarah needed more guidance and closer monitoring
  * care and also projects/tasks. She needed only initial direction with no
  * based upon department and patient needs that would change during the
  * Jozy simply needs more time in an environment
  * step forward and handle all needed tasks even when not asked.
  * listens attentively and assesses their needs, then acts in an appropriate
  * as keeping in mind the needs of the patient.
  * if an extension may be needed to complete a task.
  * thought out and appropriate.  He needed very little guidance.
  * for, diligent on obtaining resources needed to improve patient outcomes.
  * Li is assertive when he needs to be for his patient
  * Li will be needy in the beginning of the
  * overall picture of the patients' needs. At times, when Li is
  * to organize and manage time needs work but this comes with
  * is very assertive when she needs to be. This is reflected
  * Works well without needing to be "spoonfed"
  * initiative to identify and address needs, and is able to anticipate
  * rounds. She spoke up when needed to as a patient's advocate.
  * learned to balance between the need for brevity with just the
  * to ask for what she needed to complete her work.  She
  * an obstacle to gathering the needed data for her project, she
  * fundamentals were sound, she just needed more opportunities to gain experience
  * willing to help others when needed and got along well with
  * Does not need to be micro-managed, looks things
  * allowed for additional questioning if needed. When communicating with nursing staff
  * and appropriately sought feedback when needed.
  * have to tell Jenny what needs to be done and when.
  * To conduct her research, she needed to work with members of
  * listened carefully to her patient needs, spoke kindly to answer their
  * willing to do whatever was needed.
  * rotation with another student who needed much more guidance than her
  * but asking appropriate questions when needed.  Jennifer had very good drug
  * her discussion to meet the needs of the patient. She always
  * has good foundation and skills needed for residency program
  * as a result and I needed to talk with him about
  * Justin needed little direction.
  * an aging demographic, Justin would need to be extremely patient and
  * at identifying things that she needed to improve, always asked how
  * Laura needs very little hand holding.  A
  * techniques depending upon the individualized needs of the patient.  Laura also
  * with just virtual assistance as needed; She was timely and mastered
  * and very sensitive to  the needs of others. I would call
  *     of her patients and their needs.

  * my expectations with these and needed very little feedback.
  * Finds what she needs to, with ease.  Collaborated well
  * She listened actively to their needs and concerns.
  * of what she wants and needs to address, and she does
  * She needed a lot of guidance initially,
  * Shalin needs to remarkably improve in this
  * learner. She recognizes what she needs to learn and reads about
  * taking notes.   Occasionally, she may need a reminder on completing task
  * clear, concise with rare corrections needed on her initial draft. Her
  * communication style to the differing needs of patients and members of
  * were made.  Some intervention was needed, but this was appropriate for
  * ability to be assertive, but needs to gain the confidence, which
  * but going forward he will need to work and make some
  * questions correctly - some questions needing prompting. At the midpoint, we
  * he was assertive when he needed to be regarding certain patient
  * they may fit with his needs and desires.
  * tasks were complete without us needing to provide direction. Lucas was
  * to acquire the information he needs to complete an accurate medication
  * as well as which issues needed attending decisions vs. resident decisions,
  * independently.  The peers that he needs to communicate with are physicians,
  * completed all the required forms needing only minor edits.  We were
  * and differentiating which phone calls need to be transferred to the
  * presentations. Additionally, she prioritized the needs of her patients to ensure
  * sections pertaining to factors that need to be assessed to analyze
  * summarize her recommendations.  Formal presentations need some work.  Joel knew her
  * resources and gets feedback if needed.
  * onsite to complete what is needed to be done.
  * and input data with minimal need for assistance.
  * to continue the cefotaxime without need for preceptor intervention.
  * was able to identify the need for more information or intervention,
  * identification of the questions she needed to answer (aka information she
  * the day and tasks that needed to be completed.
  * second rotation overall.  While she needed some time to get oriented
  * any environment. Once help is needed, she focuses on finding solutions.
  * approach or clarify information as needed.
  * Elizabeth is assertive when she needs to be and takes initiative
  * her daily activities.  When she needs it she seeks clarification and
  * point where further instruction was needed on project.
  * with them and address their needs.
  * and the types of details needed to make a clinical assessment,
  * of adapting to the unique needs of my patient population and
  * I did not feel the need to review electronic communications before
  * Mary doesn't need someone looking over her shoulder
  * 2nd rotation I believe. She needed a good deal of coaching
  * time intensive rotational experience.  Students need to organize and manage their
  * and effective at it when needed
  * This will involve identifying the need of the audience (patient, physician,
  * is able to self-identify his needs and the needs of the
  *     request (question asked) and actual need.

    He and I discussed that part
  * has an excellent foundation, she needed to keep building on it
  * can tell patients what they need to hear - give them
  * the day.  Leah knows what needs to be done without direction
  * therapeutic issues and problems that needed to be presented to me
  * with others when she doesn't need anything from them. She works
  * ability to articulate what she needs and what her expectations are
  * was given several cases that needed the application of pharmacotherapy skills.
  * reason, and communicates what she needs to her immediate supervisor and
  * had encountered before.  She certainly needed some start up time to
  * works through problems well.  She needs to become more comfortable with
  * priority and will do what needs to be done to address
  * she can see the greatest need.  In all of these things
  * She is not someone who needs her hands held, but if
  * Hoang's writing skills need improvement.  Due to her being
  * residents. Additionally, she did not need a lot of direction to
  * any additional information that was needed. I think his experience in

<!-- end of list -->

## Required


```r
text <- str_extract(lor$comment, "([^ ]+ +){0,5}requir([^ ]*)( +[^ ]+){0,5}")
temp <- is.na(text)
text <- text[temp == FALSE]
pander(as.list(text))
```



  * Kristi requires very little instruction and does
  * time. An extension was never required for any of her assignments.
  * difference appropriately.  These written materials required very few edits.
  * with me. With minimal modeling required, Monica was able to effectively
  * and each time the notes required very little or no editing.
  * I required Krishna and I have weekly
  * This rotation required frequent correspondence through email and
  * Lauren is very timid and required some coaxing on when and
  * it. At times she did require my guidance to finalize her
  * daily for her patient care requirements of the rotation.
  * initiative to go beyond my requirements and worked up patients and
  * learning groups.  Each situation has required strong communication skills and working
  * In addition to required topic discussions and presentations, Kirby
  * with healthcare team members that required  minimal revisions from the preceptor
  * Justin has worked on projects requiring multidisciplinary collaboration. On points of
  * He has been required to speak at multidisciplinary meetings
  * Fulton Hospital was multifaceted and required a great deal of multi-tasking.
  * Our Emergency Medicine rotation requires students to spend much of
  * rotation, Ohio Northern University's APPE requirements, and longitudinal projects for the
  * regional medical center.  This task required Mark to identify and distill
  * volunteering mentor (it was not required, but he volunteered to help
  * and meetings to attend that required multitasking and prioritization.  Never once,
  * assigned more complex cases that required in-depth review. She was able
  * Natalia required minimal guidance after the first
  * order to meet the minimum-possible requirements to get through a patient-counseling
  * her own knowledge gaps without requiring direction. Is also able to
  * Katie is a self-starter and requires no motivation to get things
  * student professionalism committee and was required to sit in on many
  * our grand rounds, students are required to develop an abstract.  Ms.
  * followed through with actions that required follow-up phone calls or additional
  * working in the clinic.  It requires them to take themselves away
  * beginning of the rotation, Sarah required more guidance in analyzing patient
  * able to determine the flow, requirements, and time frames for her
  * skill, Jozy excelled. She consistently required very little intervention when speaking
  * never a single incident that required intervention on this topic. Jozy
  * direct the group to complete required projects.
  * very independent, knowing what is required on the rotation. He is
  * zone.  She really did not require any handholding.  I witnessed her
  * students.  Our service learning project required us to help our community
  * fourth year, they are only required to follow up on four
  * my rotation. Outside of her required activities, she worked alongside her
  * of the course, students are required to conduct peer evaluations of
  * Binni's research project was interdisciplinary, requiring her to work alongside, and
  * Binni was required to write a thesis to
  * for nursing staff. His projects required minimal edits and were always
  * time on rotation.  This was required of her because of the
  * Mina's research project did not require interactions with patients.
  * treatment options, dosing, and food requirements of therapy.  She also demonstrated
  * of Mina's written projects typically required minimal revisions mostly addressing thoroughness
  * efforts were very good and required only minor revisions. Mina has
  * presentation and patient case presentation required on this rotation.
  * ability to manage many tasks required of a PGY1 residency with
  * year, and little editing was required.  This was also true of
  * staff. For formal presentations that required a laptop and or projector,
  * co-authored an IRB proposal which required minimal editing and was extremely
  * Jennifer required little edits to her notes.
  * and meet all deadlines for required projects. She was always professionally
  *     staff and myself.

    When completing her required drug information responses, Jeena to
  * Jeena completed all required assignments within the first four
  * her Drug Information rotation, I required Jeena to hand in several
  * perform above and beyond minimal requirements.  Because of Amber's influence, this
  * very independent and resourceful and requires minimal supervision.
  * accurate, and professional. Her writing requires little editing.
  * to critically think. He was required to come up with interventions
  * assistance when further clarification is required.
  * frequently a laborious task that requires immense patience and diligence. Everyone
  * Christine requires minimal direction and instruction. She
  * Likhitha required little to no direction when
  * draft was well written and required only minor edits in her
  * Luke works independently and requires very little assistance to complete
  * his work and see each required patient in a timely manner.
  * Luke independently completed all the required forms needing only minor edits.
  * the many tasks that are required during a PGY1 residency.
  * based on her current rotation requirements.  This shows a recognition of
  * much thought and effort and required fewer edits and suggestions than
  * with a private attending physician required Rosa to have enough confidence
  * her time to meet the requirements. She does not hesitate to
  * were several group projects that required exceptional team work.  She was
  * that project while exceeding the requirements  of my rotation.  I was
  * She is very clear and requires little editing.  She is quite
  * even though this was not required of him. He did a
  * with writing that would be required during a residency.
  * a self-directed, self-motivated learner. He requires very little external motivation.
  * confusion earlier this year regarding requirements at an APPE site, she
  * notes for the patient chart required almost no feedback from the
  * was assigned a project that required her to formulate a hospital
  * ability to identify situations that require intervention and independently resolve the
  * Olajumoke was required to answer phones and talk
  * L fluid restriction and only required 40 mg daily of oral
  * During the rotation, Elizabeth was required to communicate with clinical staff.
  * and a Power Point presentation) required little modification and were shared
  * pass/no pass assessment system that requires a score of 90% or
  * on small research project which required her to work on data
  * Elizabeth requires little direction with assignments, projects,
  * of her patients and rotation requirements.
  * Abeer required some structure at first in
  * to create practice questions that require the student to apply their
  * to complete all of the requirements of a special project in
  * on a research project that required a manuscript, I was able
  * Pharmacotherapy course. Although they have required some editing on my part
  * my APPE rotation he was required to present two talks on
  * Student Chapter (SCCP), Brandon was required to stay organized and help
  * his APPE rotation, he was required to deliver two 15 minute
  * projects on top of his required schoolwork.  He and I discussed
  * of pharmacy clinical specialists.  She required very little coaching while developing
  * was able to provide the required components.

<!-- end of list -->

## Consistent
