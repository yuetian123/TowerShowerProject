using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class NPC : MonoBehaviour
{
    public GameObject info;
    public NPCType state;
    public GameObject dis;
    private Transform player;
    private Quaternion rotation;
    private void Awake()
    {
        player = GameObject.FindGameObjectWithTag("Player").GetComponent<Transform>();
    }
    private void Update()
    {
        if (-5 < transform.localEulerAngles.x)
        {
            transform.localEulerAngles = new Vector3(0,transform.localEulerAngles.y,0);
        }
        if (Vector3.Distance(transform.position,player.position) < 2)
        {
            rotation = Quaternion.LookRotation(player.position - transform.position);
            transform.rotation = Quaternion.Slerp(transform.rotation, rotation, 3 * Time.deltaTime);
        }
        if (Vector3.Distance(transform.position,player.position) < 2 && !info.active)
        {
            info.SetActive(!info.active);
        }
        if (Vector3.Distance(transform.position, player.position) > 2 && info.active)
        {
            info.SetActive(!info.active);
        }
        if (Vector3.Distance(transform.position, player.position) < 2 && Input.GetKeyDown(KeyCode.F))
        {
            Speak.Instance.StartSpeak(state);
            dis.SetActive(false);
        }
    }
}
public enum NPCType
{
    Cat,//小猫
    Dog,//小狗
    Squirrel,//松鼠
    Bird,//小鸟
    Shh,//刺猬

    NULL = 999
    
}